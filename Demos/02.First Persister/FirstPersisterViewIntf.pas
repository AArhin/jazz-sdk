unit FirstPersisterViewIntf;

interface

uses
  JazzDatabaseMechanismIntf,
  JazzViewIntf,
  DB;

type
  IPersonListStatusView = interface(IObjectListTypeView)
    ['{B28CE48B-0A1A-42AA-91F6-6065EBA21208}']
    function GetCaption: string;
    procedure SetCaption(const Value: string);
    property Caption: string read GetCaption write SetCaption;
  end;


implementation

end.
