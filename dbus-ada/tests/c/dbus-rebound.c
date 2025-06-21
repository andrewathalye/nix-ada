/*
 * D_Bus/Ada - An Ada binding to D-Bus
 *
 * Copyright (C) 2011  Reto Buerki <reet@codelabs.ch>
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301,
 * USA.
 */

#include <glib.h>
#include <glib/gstdio.h>

#include <dbus/dbus.h>
#include <dbus/dbus-glib-lowlevel.h>

#define NAME "dbus.ada.server"
#define ADDR "./dbusada.addr"

static DBusServer *server = NULL;
static GMainLoop *loop = NULL;

static void assert_no_error (const DBusError *e)
{
	if (G_UNLIKELY (dbus_error_is_set (e)))
	{
		g_error ("Got error: %s: %s", e->name, e->message);
	}
}

static DBusHandlerResult message_handler (DBusConnection *conn,
		DBusMessage *msg, void *data)
{
	DBusMessage* reply;
	dbus_uint32_t serial, r_serial = 0;

	const dbus_uint32_t s_serial = dbus_message_get_serial (msg);
	const char *dest = dbus_message_get_sender (msg);

	reply = dbus_message_copy (msg);

	if (!dbus_message_set_reply_serial (reply, s_serial))
	{
		g_error ("Could not set reply serial");
	}

	if (!dbus_message_set_sender (reply, NAME))
	{
		g_error ("Could not set sender name");
	}

	if (!dbus_message_set_destination (reply, dest))
	{
		g_error ("Could not set destination");
	}

	/* send the reply && flush the connection */
	if (!dbus_connection_send (conn, reply, &serial))
	{
		g_error ("Out of memory");
	}
	dbus_connection_flush (conn);

	dbus_message_unref (reply);

	return DBUS_HANDLER_RESULT_HANDLED;
}

static void handle_connection (DBusServer *server,
		DBusConnection *conn, void *data)
{
	DBusObjectPathVTable vtable = {
		NULL, &message_handler, NULL, NULL, NULL, NULL
	};

	if (!dbus_connection_register_object_path (conn, "/", &vtable, NULL))
	{
		g_error ("Can't register local test object");
		return;
	}

	dbus_connection_ref(conn);
	dbus_connection_setup_with_g_main(conn, NULL);
}

static void setup ()
{
	char *address;
	DBusError err;
	int fd, len;

	dbus_error_init (&err);

	server = dbus_server_listen ("unix:tmpdir=/tmp", &err);
	assert_no_error (&err);
	g_assert (server != NULL);

	address = dbus_server_get_address (server);

	if (!g_file_set_contents (ADDR, address, -1, NULL))
	{
		g_error ("Can't create server address file");
	}
	dbus_free (address);

	dbus_server_setup_with_g_main (server, NULL);
	dbus_server_set_new_connection_function (server,
			handle_connection, NULL, NULL);
}

static void teardown ()
{
	if (server != NULL)
	{
		dbus_server_disconnect (server);
		dbus_server_unref (server);
		server = NULL;
	}

	g_unlink (ADDR);
}

void sigterm ()
{
	g_main_loop_quit (loop);
}

int main (int argc, char **argv)
{
	signal(SIGTERM, sigterm);

	loop = g_main_loop_new (NULL, FALSE);
	setup ();
	g_main_loop_run (loop);

	teardown();
}
