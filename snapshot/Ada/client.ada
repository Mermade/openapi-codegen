with IO.OpenAPI.Api.Clients;
with IO.OpenAPI.Api.Models;
with Swagger;
with Util.Http.Clients.Curl;
with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Calendar.Formatting;
with Ada.Exceptions;
procedure IO.OpenAPI.Api.Client is

   use Ada.Text_IO;

   procedure Usage;

   Server    : constant Swagger.UString := Swagger.To_UString ("http://localhost:8080/v2");
   Arg_Count : constant Natural := Ada.Command_Line.Argument_Count;
   Arg       : Positive := 1;

   procedure Usage is
   begin
      Put_Line ("Usage: swagger_petstore {params}...");
   end Usage;

begin
   if Arg_Count <= 1 then
      Usage;
      return;
   end if;
   Util.Http.Clients.Curl.Register;
   declare
      Command : constant String := Ada.Command_Line.Argument (Arg);
      Item    : constant String := Ada.Command_Line.Argument (Arg + 1);
      C       : IO.OpenAPI.Api.Clients.Client_Type;
   begin
      C.Set_Server (Server);
      Arg := Arg + 2;

   exception
      when E : Constraint_Error =>
         Put_Line ("Constraint error raised: " & Ada.Exceptions.Exception_Message (E));

   end;
end IO.OpenAPI.Api.Client;
