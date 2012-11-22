unit JazzPersisterIntf;

interface

uses
  Classes,
  JazzClasses,
  JazzValueTypeIntf,
  JazzMappingIntf,
  JazzCriteriaIntf,
  JazzDataFactory,
  JazzMechanismIntf,
  JazzSessionIntf;

type
  IPersister = interface(IInterface)
    ['{A7118690-44BD-4D13-AE5B-350EB82CFDB1}']
    function GetDefaultMapping: IMapping;
    function GetDefaultSession: ISession;
    function GetSession(const Name: string): ISession;
    function GetSessions(const Index: Integer): ISession;
    function GetSessionsCount: Integer;

    property DefaultMapping: IMapping read GetDefaultMapping;
    property SessionsCount: Integer read GetSessionsCount;
    property Session[const Name: string]: ISession read GetSession;
    property Sessions[const Index: Integer]: ISession read GetSessions;
    property DefaultSession: ISession read GetDefaultSession;
  end;

implementation

end.



