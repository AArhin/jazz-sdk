program DemoObjectList;

uses
  FastMM4,
  Forms,
  MainForm in 'MainForm.pas' {FormMain},
  BOPerson in '..\00.Common\BO\BOPerson.pas',
  BOPersonIntf in '..\00.Common\BO\BOPersonIntf.pas',
  RandomData in '..\00.Common\RandomData.pas',
  ViewEditores in '..\00.Common\ViewEditores.pas',
  AppConsts in '..\00.Common\AppConsts.pas',
  ObjectLinkManager in '..\00.Common\ObjectLinkManager.pas',
  PersonManager in '..\00.Common\PersonManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
