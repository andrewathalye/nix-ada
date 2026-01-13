--
--  Copyright (C) 2025, AdaCore
--
--  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
--
with Ada.Characters.Latin_1;
with Ada.Containers.Hashed_Sets;
with Ada.Strings.Hash_Case_Insensitive;
with Ada.Strings.Wide_Wide_Fixed;

with Langkit_Support.Text;

with Libadalang.Common;  use Libadalang.Common;
with Laltools.Common;    use Laltools.Common;

with LAL_Refactor.Tools; use LAL_Refactor.Tools;

package body LAL_Refactor.Extract_Variable is

   Tool_Name : constant String := "Extract Variable";

   Spaces    : constant String (1 .. 80) := (others => ' ');

   function Is_Expr (Node : Ada_Node'Class) return Boolean is
     (not Node.Is_Null and then Node.Kind in Ada_Expr);

   function Is_Stmt_List (Node : Ada_Node'Class) return Boolean is
     (not Node.Is_Null and then Node.Kind in Ada_Stmt_List_Range);

   -----------------------------------
   -- Is_Extract_Variable_Available --
   -----------------------------------

   function Is_Extract_Variable_Available
     (Unit               : Analysis_Unit;
      Section_To_Extract : in out Source_Location_Range)
      return Boolean
   is
      Start_Token : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'
             (Line   => Section_To_Extract.Start_Line,
              Column => Section_To_Extract.Start_Column));

      End_Token   : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'
             (Line   => Section_To_Extract.End_Line,
              Column => Column_Number'Max
                (1, Section_To_Extract.End_Column - 1)));

      Start_Node : Ada_Node := No_Ada_Node;
      End_Node   : Ada_Node := No_Ada_Node;
      Aux        : Ada_Node := No_Ada_Node;

      -----------------------------
      -- Is_Selection_Valid_Math --
      -----------------------------

      function Is_Selection_Valid_Math return Boolean;
      --  Moving up the tree from the start of selection and checking that
      --  we did not split a half more prioritized math operation.

      function Is_Selection_Valid_Math return Boolean
      is
         Right_Bound : constant Source_Location :=
           End_Node.Sloc_Range.End_Sloc;
         Subtree     : Ada_Node := Start_Node;
         Parent      : Ada_Node;
         Priority    : Natural := Natural'Last;
         Tmp         : Natural;

         -- Get_Priority --

         function Get_Priority (Node : Ada_Node) return Natural;
         --  Returns priority of the operation

         function Get_Priority (Node : Ada_Node) return Natural is
         begin
            case Node.As_Bin_Op.F_Op is
               when Ada_Op_In | Ada_Op_Not_In =>
                  return 7;

               when Ada_Op_And_Then
                  | Ada_Op_Or_Else
                  | Ada_Op_And
                  | Ada_Op_Or
                  | Ada_Op_Xor
                  =>
                  return 6;

               when Ada_Op_Eq
                  | Ada_Op_Neq
                  | Ada_Op_Gt
                  | Ada_Op_Gte
                  | Ada_Op_Lt
                  | Ada_Op_Lte
                  =>
                  return 5;

               when Ada_Op_Double_Dot =>
                  return 4;

               when Ada_Op_Plus | Ada_Op_Minus | Ada_Op_Concat =>
                  return 3;

               when Ada_Op_Mult | Ada_Op_Div | Ada_Op_Mod | Ada_Op_Rem =>
                  return 2;

               when Ada_Op_Pow =>
                  return 1;

               when Ada_Op_Abs | Ada_Op_Not =>
                  return 0;
            end case;
         end Get_Priority;

      begin
         if Kind (Start_Node) in Ada_Op
           or else Kind (End_Node) in Ada_Op
         then
            --  Do not allow selecting the operator as the start/end point
            return False;
         end if;

         while Subtree.Sloc_Range.End_Sloc < Right_Bound
           and then Is_Expr (Subtree)
         loop
            Parent := Subtree.Parent;

            if Kind (Parent) in Ada_Bin_Op then
               if Start_Node.Sloc_Range.Start_Sloc >
                 Parent.Sloc_Range.Start_Sloc
                 or else End_Node.Sloc_Range.End_Sloc <
                   Parent.Sloc_Range.End_Sloc
               then
                  --  We reached a new operation. Get its priority.
                  Tmp := Get_Priority (Parent);

                  --  It should have the same priority or higher.
                  --  In this case, for the expression like "3 * `4 + 5`"
                  --  (whene `...` is selection) we have 3 * 4 as left and
                  --  X + 5 as right, and left operation is "*" right: "+" with
                  --  less priority, so extraction is not allowed.
                  if Priority < Tmp then
                     return False;
                  end if;
               end if;

               Priority   := Tmp;
            end if;

            Subtree := Parent;
         end loop;

         return Subtree.Sloc_Range.End_Sloc = Right_Bound;
      end Is_Selection_Valid_Math;

   begin
      Start_Node := Lookup (Unit, Start_Token, Forward);

      if Start_Node.Is_Null then
         return False;
      end if;

      End_Node := Lookup (Unit, End_Token, Backward);

      if End_Node.Is_Null then
         return False;
      end if;

      Aux := Find_Parent
        (Find_First_Common_Parent
           (Start_Node, End_Node, With_Self => True),
         Is_Expr'Access);

      if Aux.Is_Null
        or else Aux.Parent.Is_Null
      then
         return False;
      end if;

      if (Aux.Sloc_Range.Start_Sloc /= Start_Node.Sloc_Range.Start_Sloc
          or else Aux.Sloc_Range.End_Sloc /= End_Node.Sloc_Range.End_Sloc)
        and then not Is_Selection_Valid_Math
      then
         return False;
      end if;

      Section_To_Extract :=
        (Start_Line   => Start_Node.Sloc_Range.Start_Sloc.Line,
         Start_Column => Start_Node.Sloc_Range.Start_Sloc.Column,
         End_Line     => End_Node.Sloc_Range.End_Sloc.Line,
         End_Column   => End_Node.Sloc_Range.End_Sloc.Column);

      return True;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            LAL_Refactor.Is_Refactoring_Tool_Available_Default_Error_Message
              (Tool_Name));
         return False;
   end Is_Extract_Variable_Available;

   -------------------------------------
   -- Default_Extracted_Variable_Name --
   -------------------------------------

   function Default_Extracted_Variable_Name
     (Unit     : Analysis_Unit;
      Location : Source_Location)
      return Unbounded_String
   is
      use Ada.Containers;
      use Ada.Strings;
      use Langkit_Support.Text;

      Node                     : constant Ada_Node :=
        Unit.Root.Lookup (Location);
      Declarative_Parts : constant Ada_List_Hashed_Set :=
        Find_Visible_Declarative_Parts (Node);

      function Hash (US : Unbounded_String) return Hash_Type is
        (Ada.Strings.Hash_Case_Insensitive (To_String (US)));

      package Unbounded_String_Hash_Sets is new
        Ada.Containers.Hashed_Sets
          (Element_Type        => Unbounded_String,
           Hash                => Hash,
           Equivalent_Elements => "=",
           "="                 => "=");

      subtype Unbounded_String_Ordered_Set is Unbounded_String_Hash_Sets.Set;

      --  Names in Nearest_Declarative_Part
      Names : Unbounded_String_Ordered_Set;

      --  Default name when there are no collisions
      Target_Name     : constant Unbounded_String :=
        To_Unbounded_String ("Extracted");
      Aux_Target_Name : Unbounded_String := Target_Name;
      Counter         : Positive := 1;

   begin
      --  Find all names in Declarative_Parts
      for Scope of Declarative_Parts loop
         for Decl of Scope.Children loop
            if Decl.Kind in Ada_Basic_Decl then
               Names.Include
                 (To_Unbounded_String
                    (To_UTF8 (Decl.As_Basic_Decl.P_Defining_Name.Text)));
            end if;
         end loop;
      end loop;

      --  The extracted name will have an index if the default name
      --  (Target_Name) already exists. The full signature is not compared,
      --  only the name.

      while Names.Contains (Aux_Target_Name) loop
         Aux_Target_Name :=
           Target_Name
           & ("_" & Trim (To_Unbounded_String (Counter'Image), Both));
         Counter := Counter + 1;
      end loop;

      return Aux_Target_Name;
   end Default_Extracted_Variable_Name;

   -------------------------------
   -- Create_Variable_Extractor --
   -------------------------------

   function Create_Variable_Extractor
     (Unit               : Analysis_Unit;
      Section_To_Extract : Source_Location_Range;
      Variable_Name      : Unbounded_String)
      return Variable_Extractor
   is
      Start_Token : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'
             (Section_To_Extract.Start_Line,
              Section_To_Extract.Start_Column));
      End_Token   : constant Token_Reference :=
        Unit.Lookup_Token
          (Source_Location'
             (Section_To_Extract.End_Line,
              Column_Number'Max
                (1, Section_To_Extract.End_Column - 1)));

      Start_Node  : constant Ada_Node := Lookup (Unit, Start_Token, Forward);
      End_Node    : constant Ada_Node := Lookup (Unit, End_Token, Backward);

      Aux         : Ada_Node;
      Left        : Source_Location := No_Source_Location;
      Right       : Source_Location := No_Source_Location;

   begin
      Aux := Find_Parent
        (Find_First_Common_Parent (Start_Node, End_Node, With_Self => True),
         Is_Expr'Access);

      if Aux.Sloc_Range.Start_Sloc /= Start_Node.Sloc_Range.Start_Sloc
        or else Aux.Sloc_Range.End_Sloc /= End_Node.Sloc_Range.End_Sloc
      then
         Left  := Start_Node.Sloc_Range.Start_Sloc;
         Right := End_Node.Sloc_Range.End_Sloc;
      end if;

      return Variable_Extractor'
        (Unit,
         Aux,
         Left,
         Right,
         Variable_Name);
   end Create_Variable_Extractor;

   --------------
   -- Refactor --
   --------------

   function Refactor
     (Self           : Variable_Extractor;
      Analysis_Units : access function return Analysis_Unit_Array)
      return Refactoring_Edits
   is
      use Ada.Characters.Latin_1;

      Start_Line   : constant Line_Number :=
        (if Self.Left /= No_Source_Location
         then Self.Left.Line
         else Self.Node.Sloc_Range.Start_Line);
      End_Line     : constant Line_Number :=
        (if Self.Right /= No_Source_Location
         then Self.Right.Line
         else Self.Node.Sloc_Range.End_Line);
      Start_Column : constant Column_Number :=
        (if Self.Left /= No_Source_Location
         then Self.Left.Column
         else Self.Node.Sloc_Range.Start_Column);
      End_Column   : constant Column_Number :=
        (if Self.Right /= No_Source_Location
         then Self.Right.Column
         else Self.Node.Sloc_Range.End_Column);

      Enclosing_Declarative_Part : constant Declarative_Part :=
        Get_Enclosing_Declarative_Part (Self.Node);

      --  Looking for the first semicolon before
      Aux_Token       : constant Token_Reference :=
        Find (Self.Node.Token_Start, Ada_Semicolon, Backward);

      Expression_Type : Base_Type_Decl;
      Insert_Location : Source_Location_Range;
      Text_Edits      : Text_Edit_Ordered_Set;
      Edits           : Refactoring_Edits;

      Indent_Length   : Natural := 2;
      Line            : Line_Number;
      Column          : Column_Number;
      Prefix          : Unbounded_String;
      After_Semicolon : Boolean := True;

      function First_LF_If_Needed return String;
      --  Returns LF if Line-1 is empty line and empty string if not

      function Last_LF_If_Needed return String;
      --  Returns LF if Line+1 is empty line and empty string if not

      function Get_Variable_Type return Unbounded_String;
      --  Returns the resulting expression type

      function Expression (Prefix_Length : Positive) return Unbounded_String;
      --  Returns extracted expression. If it is a multiline expression, it
      --  will also be indented starting from the second line.

      procedure Get_Indent_Length (Token : Token_Reference);
      --  Set Indent_Length as a Start_Column of the next node after the Token

      function Indent return String;
      --  Returms indentation string

      procedure After_Line_Position;
      --  Set Column after the last symbol in the Line

      procedure Find_Assignment_Place_In_Code;
      --  Find the place where we can set the assignment code.

      -----------------------
      -- Get_Variable_Type --
      -----------------------

      function Get_Variable_Type return Unbounded_String
      is
         Res : Unbounded_String;
         St  : constant String := "Standard.";
      begin
         Expression_Type := Self.Node.As_Expr.P_Expression_Type;

         if Expression_Type.Is_Null then
            return To_Unbounded_String ("<Insert Type>");

         else
            Res := To_Unbounded_String
              (Langkit_Support.Text.To_UTF8
                 (Expression_Type.P_Fully_Qualified_Name));

            --  Cut `Standard.` at the beginning of the type name
            if Res.Length > St'Length
              and then Res.Slice (1, St'Length) = St
            then
               Res := Res.Unbounded_Slice (St'Length + 1, Res.Length);
            end if;

            return Res;
         end if;
      end Get_Variable_Type;

      ----------------
      -- Expression --
      ----------------

      function Expression (Prefix_Length : Positive) return Unbounded_String
      is
         use Langkit_Support.Text;
         use Ada.Strings.Wide_Wide_Fixed;

         Start_Token : Token_Reference := Self.Node.Token_Start;
         End_Token   : Token_Reference := Self.Node.Token_End;

         Result      : Unbounded_String;

         SL : Line_Number   := Start_Line;
         EL : Line_Number   := End_Line;
         SC : Column_Number := Start_Column;
         EC : Column_Number := End_Column;

      begin
         if Self.Left = No_Source_Location
           and then Self.Node.Kind = Ada_Paren_Expr
         then
            --  Cut '(' and ')' around the expression
            Start_Token := Next_Non_Whitespace (Start_Token, Forward);
            End_Token   := Next_Non_Whitespace (End_Token, Backward);

            SL := Start_Token.Data.Sloc_Range.Start_Line;
            EL := End_Token.Data.Sloc_Range.End_Line;
            SC := Start_Token.Data.Sloc_Range.Start_Column;
            EC := End_Token.Data.Sloc_Range.End_Column;
         end if;

         if EL - SL + 1 = 1 then
            declare
               Txt   : constant Text_Type :=
                 Self.Unit.Get_Line (Positive (SL));
               Start : constant Positive := Positive
                 (Txt'First + Positive (SC) - 1);
               Last  : constant Positive := Positive
                 (Txt'First + Positive (EC)   - 2);

               Result : Unbounded_String;
            begin
               Result := To_Unbounded_String
                 (To_UTF8 (Txt (Start .. Last)) & ";");

               --  Use one line if the assignment is less than 79
               if Prefix_Length + Result.Length < 78 then
                  return " " & Result;
               else
                  return LF & Indent & "  " & Result;
               end if;
            end;

         else
            declare
               Txt   : constant Text_Type :=
                 Self.Unit.Get_Line (Positive (SL));
               Start : constant Positive := Positive
                 (Txt'First + Positive (SC) - 1);

            begin
               Result.Append
                 (LF & Indent & "  " & To_UTF8 (Txt (Start .. Txt'Last)) & LF);
            end;

            for Line_Number in SL + 1 .. EL - 1 loop
               declare
                  Txt   : constant Text_Type :=
                    Self.Unit.Get_Line (Positive (Line_Number));
                  Start : constant Positive := Index_Non_Blank (Txt);

               begin
                  Result.Append
                    (Indent & "    " & To_UTF8 (Txt (Start .. Txt'Last)) & LF);
               end;
            end loop;

            declare
               Txt   : constant Text_Type :=
                 Self.Unit.Get_Line (Positive (EL));
               Start : constant Positive := Index_Non_Blank (Txt);
               Last  : constant Positive := Positive
                 (Txt'First + Positive (EC) - 2);

            begin
               Result.Append
                 (Indent & "    " & To_UTF8 (Txt (Start .. Last)) & ";");
            end;
         end if;

         return Result;
      end Expression;

      ------------------------
      -- First_LF_If_Needed --
      ------------------------

      function First_LF_If_Needed return String is
         Token : constant Token_Reference := Previous
           (Self.Unit.Lookup_Token
              (Source_Location'
                   (Line   => Insert_Location.Start_Line,
                    Column => Insert_Location.Start_Column)));
         Node  : constant Ada_Node := Lookup (Self.Unit, Token, Backward);

      begin
         if Node = No_Ada_Node
           or else Node.Token_Start.Data.Sloc_Range.Start_Line >
             Insert_Location.Start_Line - 2
         then
            return "" & LF;
         else
            return "";
         end if;
      end First_LF_If_Needed;

      -----------------------
      -- Last_LF_If_Needed --
      -----------------------

      function Last_LF_If_Needed return String is
         Token : constant Token_Reference :=
           Self.Unit.Lookup_Token
             (Source_Location'
                (Line   => Insert_Location.End_Line + 1,
                 Column => 1));
         Node  : constant Ada_Node := Lookup (Self.Unit, Token, Forward);

      begin
         if Node = No_Ada_Node
           or else Node.Token_Start.Data.Sloc_Range.Start_Line <=
             Insert_Location.End_Line + 1
         then
            return "" & LF;
         else
            return "";
         end if;
      end Last_LF_If_Needed;

      -----------------------
      -- Get_Indent_Length --
      -----------------------

      procedure Get_Indent_Length (Token : Token_Reference)
      is
         Node : constant Ada_Node := Lookup (Self.Unit, Token, Forward);
      begin
         if Node = No_Ada_Node then
            Indent_Length := 2;
         else
            Indent_Length := Natural
              (Node.Token_Start.Data.Sloc_Range.Start_Column) - 1;
         end if;
      end Get_Indent_Length;

      ------------
      -- Indent --
      ------------

      function Indent return String is
      begin
         return Spaces (1 .. Integer'Min (Spaces'Last, Indent_Length));
      end Indent;

      -------------------------
      -- After_Line_Position --
      -------------------------

      procedure After_Line_Position is
         use Langkit_Support.Text;
         Txt : constant Text_Type :=
           Self.Unit.Get_Line (Positive (Line));
      begin
         Column := Column_Number (Txt'Last - Txt'First + 2);
      end After_Line_Position;

      -----------------------------------
      -- Find_Assignment_Place_In_Code --
      -----------------------------------

      procedure Find_Assignment_Place_In_Code
      is
         Self_Stmt_List      : constant Ada_Node :=
           Find_Parent (Self.Node, Is_Stmt_List'Access);

         Semicolon_Stmt_List : Ada_Node;
      begin
         --  Line/column points to the previous semicolon
         Insert_Location := (Line, Line, Column, Column);

         Semicolon_Stmt_List := Find_Parent
           (Lookup
              (Self.Unit,
               Self.Unit.Lookup_Token
                 (Insert_Location.Start_Sloc), Backward),
            Is_Stmt_List'Access);

         if Self_Stmt_List /= Semicolon_Stmt_List then
            --  The previous semicolon belongs to a different statement list.
            --  Use expression's statement list start point to insert
            --  an assignment.
            Insert_Location :=
              (Start_Line   => Self_Stmt_List.Sloc_Range.Start_Line,
               Start_Column => Self_Stmt_List.Sloc_Range.Start_Column,
               End_Line     => Self_Stmt_List.Sloc_Range.Start_Line,
               End_Column   => Self_Stmt_List.Sloc_Range.Start_Column);

            --  We are not at the semicolon but on the line start instead
            After_Semicolon := False;

            --  We are at the start of the line, so we can use the position
            --  as an indent
            Indent_Length   := Integer'Max
              (Integer (Insert_Location.Start_Column) - 1, 1);
         end if;
      end Find_Assignment_Place_In_Code;

   begin
      --  Default place for inserting is after the previous semicolon
      Line     := Aux_Token.Data.Sloc_Range.End_Line;
      Column   := Aux_Token.Data.Sloc_Range.End_Column + 1;

      --  Is selected expression in the declarative part?
      if Enclosing_Declarative_Part.Sloc_Range.End_Sloc >
        Self.Node.Sloc_Range.Start_Sloc
      then
         --  Expression is in the declarative part itself
         if Aux_Token.Data.Sloc_Range.End_Sloc <=
           Enclosing_Declarative_Part.Sloc_Range.Start_Sloc
         then
            --  Expression is the first in the declarative part,
            --  add right after the declarative part start
            Line := Enclosing_Declarative_Part.Sloc_Range.Start_Line;
            After_Line_Position;

            --  Indent as the first node in the declarative part is indented
            Get_Indent_Length
              (Self.Unit.Lookup_Token (Source_Location'(Line + 1, 1)));

         else
            --  Indent as the first node after the previous semicolon is
            --  indented
            Get_Indent_Length (Next (Aux_Token));
         end if;

         Insert_Location := (Line, Line, Column, Column);

         Prefix   := LF & LF & Indent & Self.Name & " : constant " &
           Get_Variable_Type & " :=";

         Text_Edits.Insert
           ((Insert_Location,
             Prefix & Expression (Prefix.Length) & Last_LF_If_Needed));

      else
         --  Expression is in the code

         --  Calculate indentation for the variable declaration
         Get_Indent_Length
           (Self.Unit.Lookup_Token
              (Source_Location'
                   (Enclosing_Declarative_Part.Sloc_Range.Start_Line + 1, 1)));

         if Enclosing_Declarative_Part.Sloc_Range.End_Sloc >
           Aux_Token.Data.Sloc_Range.End_Sloc
           or else
             (not Expression_Type.Is_Null
              and then Expression_Type.P_Is_Array_Type)
         then
            --  The expression is the first in code after the declarative path,
            --  add assigment with the declaration
            Line := Enclosing_Declarative_Part.Sloc_Range.End_Line;
            After_Line_Position;

            --  Indent as the first node after the declarative part is indented
            Get_Indent_Length
              (Self.Unit.Lookup_Token (Source_Location'(Line + 1, 1)));

            --  Add the variable declaration at the end of the declarative part
            Insert_Location :=
              (Enclosing_Declarative_Part.Sloc_Range.End_Line,
               Enclosing_Declarative_Part.Sloc_Range.End_Line,
               1,
               1);

            Prefix := First_LF_If_Needed & Indent &
              Self.Name & " : constant " & Get_Variable_Type & " :=";

            Text_Edits.Insert
              ((Insert_Location,
                Prefix & Expression (Prefix.Length) & LF & LF));

         else
            --  Add the variable declaration at the end of the declarative part
            Insert_Location :=
              (Enclosing_Declarative_Part.Sloc_Range.End_Line,
               Enclosing_Declarative_Part.Sloc_Range.End_Line,
               1,
               1);

            Text_Edits.Insert
              ((Insert_Location,
               First_LF_If_Needed & Indent & Self.Name & " : " &
                 Get_Variable_Type & ";" & LF & LF));

            --  Indent as the first node after the semicolon is indented
            Get_Indent_Length (Next (Aux_Token));

            Find_Assignment_Place_In_Code;

            Prefix :=
              (if After_Semicolon
               then LF & LF
               else First_LF_If_Needed)
              & Indent & Self.Name & " :=";

            Text_Edits.Insert
              ((Insert_Location,
               Prefix & Expression (Prefix.Length) &
               (if After_Semicolon
                  then Last_LF_If_Needed
                  else LF & Last_LF_If_Needed & Indent)));
         end if;
      end if;

      --  Replace the expression with the variable name
      Text_Edits.Insert
        (((Start_Line, End_Line, Start_Column, End_Column), Self.Name));

      Edits.Text_Edits.Insert (Self.Unit.Get_Filename, Text_Edits);
      return Edits;

   exception
      when E : others =>
         Refactor_Trace.Trace
           (E,
            Refactoring_Tool_Refactor_Default_Error_Message (Tool_Name));

         return No_Refactoring_Edits;
   end Refactor;

end LAL_Refactor.Extract_Variable;

