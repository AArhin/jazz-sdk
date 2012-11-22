unit JazzVCLViewIntf;

interface

uses
  JazzValueTypeIntf,
  JazzViewIntf,
  ComCtrls,
  Forms;
  
type
  IFormView = interface(IObjectTypeView)
    ['{C54E65FF-5C53-4DBE-9E45-2A778DA3F00F}']
    function GetForm: TForm;
    property Form: TForm read GetForm;
  end;

  IListViewView = interface;
  IListItemView = interface(IMemberView)
    ['{5B03D664-67C6-441F-BF28-BE7B0FBD1BD2}']
    function GetCaption: string;
    function GetColumn: TListColumn;
    function GetListView: IListViewView;
    function GetMemberName: string;
    procedure SetCaption(const Value: string);
    procedure SetColumn(const Value: TListColumn);
    procedure SetListView(const Value: IListViewView);
    procedure SetMemberName(const Value: string);

    function GetText(const AObject: IObjectType): string;

    property Caption: string read GetCaption write SetCaption;
    property Column: TListColumn read GetColumn write SetColumn;
    property ListView: IListViewView read GetListView write SetListView;
    property MemberName: string read GetMemberName write SetMemberName;
  end;

  IListViewView = interface(IObjectListTypeView)
    ['{7D31761E-4528-4052-8A39-2046AC68C72C}']
    function GetCount: Integer;
    function GetItems(Index: Integer): IListItemView;
    function Add(const Column: TObject; const MemberName: string): IListItemView;
    procedure Clear;
    procedure Remove(const Index: Integer);
    property Items[Index: Integer]: IListItemView read GetItems; default;
    property Count: Integer read GetCount;
    procedure Refresh;
  end;

  ITreeViewView = interface;
  TTreeViewOnChangeEvent = procedure(Sender: TObject; const TreeViewView: ITreeViewView; const Selection: IObjectType) of object;
  TTreeNodeCaptionNeeded = procedure(Sender: TObject; const AObject: IObjectType; var ACaption: string; var AImageIndex: Integer) of object;

  ITreeViewView = interface(IObjectListTypeView)
    ['{5B228D5F-4C80-4615-92A6-2E7FA177B199}']
    function GetOnCaptionNeeded: TTreeNodeCaptionNeeded;
    function GetOnChange: TTreeViewOnChangeEvent;
    procedure SetOnCaptionNeeded(const Value: TTreeNodeCaptionNeeded);
    procedure SetOnChange(const Value: TTreeViewOnChangeEvent);

    function GetDetailID: Integer;
    function GetMasterID: string;
    procedure SetDetailID(Value: Integer);
    procedure SetMasterID(const Value: string);

    function ListCount: Integer;
    procedure AddList(const MasterList, DetailList: IObjectListType; MasterID, DetailID: string);
    procedure Clear;
    procedure RemoveList(Index: Integer);

    procedure Refresh;

    property DetailID: Integer read GetDetailID write SetDetailID;
    property MasterID: string read GetMasterID write SetMasterID;
    property OnChange: TTreeViewOnChangeEvent read GetOnChange write SetOnChange;
    property OnCaptionNeeded: TTreeNodeCaptionNeeded read GetOnCaptionNeeded write SetOnCaptionNeeded;
  end;

implementation

end.
