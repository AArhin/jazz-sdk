unit JazzDatabaseMechanismTests;

interface

uses
  JazzDatabaseMechanism,
  TestFrameWork, JazzDatabaseMechanismIntf;

type
  TDatabaseMechanismTests = class(TTestCase)
  private
    FDatabaseMechanism: IDatabaseMechanism;

  protected

    procedure SetUp; override;
    procedure TearDown; override;

  published
    // Test methods
//    procedure TestGetOnExecuteStatement;
//    procedure TestGetNotifySession;
//    procedure TestSetOnExecuteStatement;
//    procedure TestSetNotifySession;
//    procedure TestLoadMember;
//    procedure TestLoadObject;
//    procedure TestLoadObjectList;
//    procedure TestInsertObject;
//    procedure TestDeleteObject;
//    procedure TestUpdateObject;
//    procedure TestDeleteObjectList;
//    procedure TestSave;
//    procedure TestSaveObject;
//    procedure TestSaveObject;
//    procedure TestSaveObjectList;
    procedure TestFrameWork;


  end;

implementation

uses
  DBConnection;

{ TDatabaseMechanismTests }

procedure TDatabaseMechanismTests.SetUp;
begin
  FDatabaseMechanism := Connection.DatabaseMechanism;
end;

procedure TDatabaseMechanismTests.TearDown;
begin

end;

procedure TDatabaseMechanismTests.TestFrameWork;
begin
  Check(False);
end;

initialization

//  TestFramework.RegisterTest('Jazz OPF Test Suite', TConnectionSetup.Create(TDatabaseMechanismTests.Suite));

end.
