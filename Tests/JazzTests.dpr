program JazzTests;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  TestFramework,
  GUITestRunner,
  TextTestRunner,
  TestJazzObjectTable in 'TestJazzObjectTable.pas',
  JazzObjectTable in '..\DBAdapter\JazzObjectTable.pas',
  PersonBO in 'PersonBO.pas',
  DBConnection in 'DBConnection.pas',
  TestObjectListType in 'TestObjectListType.pas',
  ValueTypeTestObjectIntf in 'ValueTypeTestObjectIntf.pas',
  OneToOneObjectIntf in 'OneToOneObjectIntf.pas',
  OneToManyObjectIntf in 'OneToManyObjectIntf.pas',
  OneToOneObject in 'OneToOneObject.pas',
  OneToManyObject in 'OneToManyObject.pas',
  ValueTypeTestObject in 'ValueTypeTestObject.pas',
  JazzObjectTypeTests in 'JazzObjectTypeTests.pas',
  JazzObjectListTypeTests in 'JazzObjectListTypeTests.pas',
  JazzDatabaseMechanismTests in 'JazzDatabaseMechanismTests.pas';

{$R *.RES}

begin
  Application.Initialize;
  if IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
end.

