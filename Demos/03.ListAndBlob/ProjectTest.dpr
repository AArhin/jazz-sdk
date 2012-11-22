program ProjectTest;

uses
  Forms,
  MainForm in 'MainForm.pas' {Form1},
  ClassesIntf in 'ClassesIntf.pas',
  ClassesImpl in 'ClassesImpl.pas',
  ClassesMapping in 'ClassesMapping.pas',
  RandomData in '..\00.Common\RandomData.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
