unit JazzSelection;

interface

uses
  Classes,
  JazzSelectionIntf,
  JazzModelIntf;

type
  TSelection = class(TInterfacedObject, ISelection)
  private
    FItems: IInterfaceList;
    function GetActive: IInterface;
  protected
    function GetCount: Integer;
    function GetItems(const Index: Integer): IInterface;

    procedure Add(const Item: IInterface);
    procedure Clear;
    procedure RemoveItem(const Item: IInterface);

    property Active: IInterface read GetActive;
    property Count: Integer read GetCount;
    property Items[const Index: Integer]: IInterface read GetItems; default;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses JazzConsts;

{ TSelection }

procedure TSelection.Add(const Item: IInterface);
begin
  if FItems.IndexOf(Item) = NotFound then FItems.Add(Item);
end;

procedure TSelection.Clear;
begin
  FItems.Clear;
end;

constructor TSelection.Create;
begin
  inherited Create;
  FItems:= TInterfaceList.Create;
end;

destructor TSelection.Destroy;
begin
  Clear;
  inherited;
end;

function TSelection.GetActive: IInterface;
begin
  if Count > 0 then
    Result:= Items[Count -1]
  else
    Result:= nil;
end;

function TSelection.GetCount: Integer;
begin
  Result:= FItems.Count
end;

function TSelection.GetItems(const Index: Integer): IInterface;
begin
  Result:= FItems[Index] as IInterface;
end;

procedure TSelection.RemoveItem(const Item: IInterface);
begin
  FItems.Remove(Item);
end;

end.
