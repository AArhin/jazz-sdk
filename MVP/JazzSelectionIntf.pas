unit JazzSelectionIntf;

interface

type
  ISelection = interface(IInterface)
    ['{1CBAE1AB-B3CC-4DFC-9A50-01822FE8E49D}']
    function GetActive: IInterface;
    function GetCount: Integer;
    function GetItems(const Index: Integer): IInterface;

    procedure Add(const Item: IInterface);
    procedure Clear;
    procedure RemoveItem(const Item: IInterface);

    property Active: IInterface read GetActive;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: IInterface read GetItems; default;
  end;

implementation

end.
