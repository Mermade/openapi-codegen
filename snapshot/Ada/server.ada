with Ada.IO_Exceptions;
with AWS.Config.Set;
with Swagger.Servers.AWS;
with Swagger.Servers.Applications;
with Util.Log.Loggers;
with Util.Properties;
with IO.OpenAPI.Api.Servers;
procedure IO.OpenAPI.Api.Server is
   procedure Configure (Config : in out AWS.Config.Object);

   CONFIG_PATH  : constant String := ".properties";

   procedure Configure (Config : in out AWS.Config.Object) is
   begin
      AWS.Config.Set.Server_Port (Config, 8080);
      AWS.Config.Set.Max_Connection (Config, 8);
      AWS.Config.Set.Accept_Queue_Size (Config, 512);
   end Configure;

   App     : aliased Swagger.Servers.Applications.Application_Type;
   WS      : Swagger.Servers.AWS.AWS_Container;
   Log     : constant Util.Log.Loggers.Logger := Util.Log.Loggers.Create ("IO.OpenAPI.Api.Server");
   Props   : Util.Properties.Manager;
begin
   Props.Load_Properties (CONFIG_PATH);
   Util.Log.Loggers.Initialize (Props);

   App.Configure (Props);
   IO.OpenAPI.Api.Servers.Server_Impl.Register (App);

   WS.Configure (Configure'Access);
   WS.Register_Application ("/v2", App'Unchecked_Access);
   App.Dump_Routes (Util.Log.INFO_LEVEL);
   Log.Info ("Connect you browser to: http://localhost:8080/v2/ui/index.html");

   WS.Start;

   delay 6000.0;

exception
   when Ada.IO_Exceptions.Name_Error =>
      Log.Error ("Cannot read application configuration file {0}", CONFIG_PATH);
end IO.OpenAPI.Api.Server;
