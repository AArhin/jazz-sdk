program Project2;

uses
  Forms,
  Unit2 in 'Unit2.pas' {Form2},
  Sistema_Impl in 'Sistema_Impl.pas',
  Sistema_Intf in 'Sistema_Intf.pas',
  Sistema_Mapping in 'Sistema_Mapping.pas',
  DBUtils in 'DBUtils.pas',
  BSE_TableID_Impl in 'BSE_TableID_Impl.pas',
  BSE_TableID_Intf in 'BSE_TableID_Intf.pas',
  BSE_TableID_Mapping in 'BSE_TableID_Mapping.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
