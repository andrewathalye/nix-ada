D\_Bus/Ada
=========

The D\_Bus/Ada library provides an Ada binding to the D-Bus message bus used for
inter-process communication on most modern Linux desktop systems.


Licence
-------
```
Copyright (C) 2011-2019 Reto Buerki <reet@codelabs.ch>.
Copyright (C) 2024 Andrew Athalye
Free use of this software is granted under the terms of the GNAT Modified
General Public License (GMGPL).
```

Github
------
This is my personal fork of D\_Bus/Ada with a number of fixes, specificially for
GNAT 13 and empty containers. It is used by my DBus Binding Generator.

I hope to submit it to the original author in due time, but it’s not yet
production ready.

I don’t intend to maintain the Makefile at this time, but I won’t actively break
it either. It may or may not work.

Current Changes:
----------------
```
D_Bus.Types:
    Add type Obj_Path (with default value)
    Add type Signature

D_Bus.Arguments.Simple:
    Add type Double_Type
    Add type File_Descriptor_Type
    Add type Signature_Type
D_Bus:
    Add basic type Double
    Add basic type File_Descriptor

D_Bus.Arguments.Containers:
    Allow empty containers.

D_Bus.Connection:
    Connect_Private
    Release_Name
    Remove_Match
    Ref
    Call_No_Reply
    Unref

    No connections exit on disconnect by default.

D_Bus.G_Main:
    Remove Init
    Add type Main_Context
    Replace Start with Start (Main_Context)
    Add Quit
    Add Create, Destroy (Main_Context)

D_Bus.Connection.G_Main:
    Allow specifying context in Setup_With_G_Main
    Make `Connection` an in parameter in Setup_With_G_Main
```

Build
-----
D\_Bus/Ada may be built using Nix:

```
  $ nix build
```

Development is via Nix shells:

```
  $ nix develop
```

Dependencies: gnat, gprbuild, dbus, dbus-glib

Testing
-------
Before you install D\_Bus/Ada on your system, you might want to test the library
and verify that everything works as expected. D\_Bus/Ada contains an unit test
suite which can be run by entering the following command:

```
  $ make tests
```

Examples
--------
D\_Bus/Ada provides example code to demonstrate the usage of the D\_Bus/Ada API.
To build all examples type the following:

```
  $ make examples
```

You can start an example application like so: `obj/examples/list_names`.
