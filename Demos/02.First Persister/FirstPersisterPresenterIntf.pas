unit FirstPersisterPresenterIntf;

interface

uses
  DatabaseSchemaClassesIntf,
  FirstPersisterClassesIntf,
  JazzPresenterIntf,
  FirstPersisterViewIntf,
  JazzSessionIntf;

type
  IFirstPersisterPresenter = interface(IPresenter)
    ['{17520853-C0DA-4703-95B8-612DB645D23C}']
    function DBXSession: ISession;
    function ADOSession: ISession;
    function StreamSession: ISession;
    function Project: IFirstPersisterType;
    procedure CreateDetails;
    procedure InitializeSession;
    procedure SetDefaults;
  end;

  IDatabaseSchemaPresenter = interface(IPresenter)
    ['{212ED5EC-AA7C-40A6-AC28-99684C44FC5B}']
    function GetDatabaseSchema: IDatabaseSchemaType;
    procedure DefaultValues;
  end;


implementation

end.
