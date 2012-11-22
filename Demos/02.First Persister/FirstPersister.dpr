program FirstPersister;

uses
  Forms,
  MainForm in 'MainForm.pas' {FormMain},
  BOPerson in '..\00.Common\BO\BOPerson.pas',
  BOPersonIntf in '..\00.Common\BO\BOPersonIntf.pas',
  BOPersonMapping in '..\00.Common\BO\BOPersonMapping.pas',
  BOAddress in '..\00.Common\BO\BOAddress.pas',
  BOAddressIntf in '..\00.Common\BO\BOAddressIntf.pas',
  AppConsts in '..\00.Common\AppConsts.pas',
  BOAddressMapping in '..\00.Common\BO\BOAddressMapping.pas',
  BOPersonWithAddress in '..\00.Common\BO\BOPersonWithAddress.pas',
  BOPersonWithAddressIntf in '..\00.Common\BO\BOPersonWithAddressIntf.pas',
  RandomData in '..\00.Common\RandomData.pas',
  SetupDBConnection in '..\00.Common\SetupDBConnection.pas',
  FirstPersisterPresenter in 'FirstPersisterPresenter.pas',
  FirstPersisterClasses in 'FirstPersisterClasses.pas',
  FirstPersisterClassesIntf in 'FirstPersisterClassesIntf.pas',
  DatabaseSchemaForm in 'DatabaseSchemaForm.pas' {FormDatabaseSchema},
  DatabaseSchemaPresenter in 'DatabaseSchemaPresenter.pas',
  DatabaseSchemaClasses in 'DatabaseSchemaClasses.pas',
  DatabaseSchemaClassesIntf in 'DatabaseSchemaClassesIntf.pas',
  FirstPersisterPresenterIntf in 'FirstPersisterPresenterIntf.pas',
  FirstPersisterViewIntf in 'FirstPersisterViewIntf.pas',
  FirstPersisterView in 'FirstPersisterView.pas',
  EditPersonForm in 'EditPersonForm.pas' {FormEditPerson},
  SQLFormater in '..\00.Common\SQLFormater.pas',
  EditPersonPresenter in 'EditPersonPresenter.pas',
  EditPersonPresenterIntf in 'EditPersonPresenterIntf.pas';

{$R *.res}

begin
  Application.Initialize;
  TFirstPersisterPresenter.Create; 
  Application.Run;
end.
