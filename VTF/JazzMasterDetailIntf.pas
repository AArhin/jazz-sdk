unit JazzMasterDetailIntf;

interface

type
  IDetail = interface;
  IMaster = interface(IInterface)
    ['{256571DC-4FA3-4124-9E7B-4993A5F28EF2}']
    function AddDetail(const Detail: IInterface): Integer;
    function DetailCount: Integer;
    function GetDetails(const Index: Integer): IDetail;

    procedure ItemChanged(const Item: IInterface);
    procedure RemoveDetail(const Detail: IInterface);
    property Details[const Index: Integer]: IDetail read GetDetails;
  end;

  IDetail = interface(IInterface)
    ['{5E19AE5B-E925-4641-8015-1023C9451BB6}']
    function GetMaster: IMaster;
    function GetMasterMember: string;

    procedure SetMaster(const AMaster: IInterface; AMasterMember: string);
    procedure UpdateMasterLink(const Item: IInterface);

    property Master: IMaster read GetMaster;
    property MasterMember: string read GetMasterMember;
  end;


implementation

end.
