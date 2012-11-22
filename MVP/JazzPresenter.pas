unit JazzPresenter;

interface

uses
  Classes,
  JazzClasses,
  JazzPresenterIntf,
  JazzModelIntf,
  JazzViewIntf,
  JazzValueTypeIntf,
  JazzObserverIntf,
  JazzTypes,
  JazzMasterDetailIntf,
  JazzNotifyIntf,
  JazzInteractorIntf;

type
  TPresenterClass = class of TPresenter;
  TPresenter = class(TCustomObject, IPresenter, IMaster, IDetail)
  private
    FDetails: IInterfaceList;
    FInteractor: IInteractorList;
    FMaster: Pointer;
    FMasterMember: string;
    FModel: IModel;
    FView: IView;
  protected
    // IPresenter
    function GetInteractor: IInteractorList; virtual;
    function GetModel: IModel;
    function GetView: IView;
    procedure SetModel(const Value: IModel);
    procedure SetView(const Value: IView);

    property Interactor: IInteractorList read GetInteractor;
    property Model: IModel read GetModel write SetModel;
    property View: IView read GetView write SetView;

    // IMaster
    function AddDetail(const Detail: IInterface): Integer;
    function DetailCount: Integer;
    function GetDetails(const Index: Integer): IDetail;
    procedure ItemChanged(const Item: IInterface);
    procedure RemoveDetail(const Detail: IInterface);
    property Details[const Index: Integer]: IDetail read GetDetails;

    // IDetail
    function GetMaster: IMaster;
    function GetMasterMember: string;
    procedure SetMaster(const AMaster: IInterface; AMasterMember: string);
    procedure UpdateMasterLink(const Item: IInterface); virtual;
    property Master: IMaster read GetMaster;
    property MasterMember: string read GetMasterMember;
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TValueTypePresenter = class(TPresenter, IValueTypePresenter);
  TMemberPresenter = class(TValueTypePresenter, IMemberPresenter)
  protected
    procedure UpdateMasterLink(const Item: IInterface); override;
  end;

  TMemoPresenter = class(TMemberPresenter)
  protected
  end;

  TCompositePresenter = class(TValueTypePresenter, ICompositePresenter)
  private
    FItems: IInterfaceList;
  protected
    // ICompositePresenter
    function Add(const PresenterItem: IPresenter): Integer;
    function GetItems(const Index: Integer): IPresenter;
    function IndexOf(const PresenterItem: IPresenter): Integer;
    procedure Clear;
    procedure Delete(const Index: Integer);
    property Items[const Index: Integer]: IPresenter read GetItems; default;
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
    destructor Destroy; override;
  end;

  TCustomObjectPresenter = class(TCompositePresenter)
  end;

  TObjectPresenter = class(TCompositePresenter)
  end;

  TCustomObjectListPresenter = class(TCompositePresenter, IObjectListPresenter)
  end;

  TObjectListPresenter = class(TCustomObjectListPresenter, IObjectListPresenter)
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
  end;

  TBooleanPresenter = class(TMemberPresenter)
  public
    constructor Create(const AOwner: IInterface; const Model: IModel; const View: IView); override;
  end;

implementation

uses SysUtils, JazzSubjectIntf, JazzConsts, JazzMvpNotify, JazzInteractor;

{ TPresenter }

function TPresenter.GetModel: IModel;
begin
  Result:= FModel;
end;

function TPresenter.GetView: IView;
begin
  Result:= FView;
end;

procedure TPresenter.SetModel(const Value: IModel);
begin
  FModel:= Value;
end;

procedure TPresenter.SetView(const Value: IView);
begin
  FView:= Value;
end;

constructor TCompositePresenter.Create(const AOwner: IInterface; const
  Model: IModel; const View: IView);
begin
  inherited Create(AOwner, Model, View);
  FItems:= TInterfaceList.Create;
end;

destructor TCompositePresenter.Destroy;
begin
  Clear;
  FItems:= nil;
  inherited;
end;

{ TPresenterList }

function TCompositePresenter.Add(const PresenterItem: IPresenter): Integer;
begin
  Result:= FItems.Add(PresenterItem);
end;

procedure TCompositePresenter.Clear;
begin
  if FItems <> nil then FItems.Clear;
end;

procedure TCompositePresenter.Delete(const Index: Integer);
begin
  FItems.Delete(Index);
end;

function TCompositePresenter.GetItems(const Index: Integer):
  IPresenter;
begin
  Result:= FItems[Index] as IPresenter;
end;

function TCompositePresenter.IndexOf(const PresenterItem: IPresenter): Integer;
begin
  Result:= FItems.IndexOf(PresenterItem);
end;

constructor TPresenter.Create(const AOwner: IInterface; const Model: IModel;
  const View: IView);
begin
  inherited Create(AOwner);
  SetModel(Model);
  SetView(View);
  FInteractor:= TInteractorList.Create(Self, Model, View);

  if Model.ObjectValue <> nil then
    (Model.ObjectValue as IValueType).Subject.Attach(View as IObserver);
end;

function TPresenter.AddDetail(const Detail: IInterface): Integer;
begin
  if FDetails = nil then FDetails:= TInterfaceList.Create;
  Result:= FDetails.Add(Detail);
end;

function TPresenter.DetailCount: Integer;
begin
  if FDetails <> nil then Result:= FDetails.Count else Result:= 0; 
end;

function TPresenter.GetDetails(const Index: Integer): IDetail;
begin
  if FDetails <> nil then Result:= FDetails[Index] as IDetail else Result:= nil; 
end;

function TPresenter.GetMaster: IMaster;
begin
  Result:= IMaster(FMaster);
end;

function TPresenter.GetMasterMember: string;
begin
  Result:= FMasterMember;
end;

procedure TPresenter.ItemChanged(const Item: IInterface);
var
  I: Integer;
begin
  if FDetails <> nil then
  begin
    for I:= 0 to FDetails.Count -1 do
      (FDetails[I] as IDetail).UpdateMasterLink(Item);
  end;
end;

procedure TPresenter.RemoveDetail(const Detail: IInterface);
begin
  if FDetails <> nil then FDetails.Remove(Detail);
end;

procedure TPresenter.SetMaster(const AMaster: IInterface;
  AMasterMember: string);
var
  LMaster: IMaster;
begin
  if Supports(AMaster, IMaster, LMaster) then
  begin
    FMaster:= Pointer(LMaster);
    LMaster.AddDetail(Self);
    FMasterMember:= AMasterMember;
  end
  else
    raise exception.Create(SIntfNotSupported);
end;

procedure TPresenter.UpdateMasterLink(const Item: IInterface);
var
  LObjectList: IObjectListType;
  LView: IObserver;
begin
  if Supports(View, IObserver, LView) then
  begin
    if Model.ObjectValue <> nil then
      (Model.ObjectValue as IValueType).Subject.Detach(LView);

    if Item <> nil then
    begin
      LObjectList:= (Item as IObjectType).Member[MasterMember] as IObjectListType;
      if LObjectList <> nil then
      begin
        Model.ObjectValue:= LObjectList;
       (Model.ObjectValue as IValueType).Subject.Attach(LView);
      end;
    end;
  end;
end;

destructor TPresenter.Destroy;
begin
  FInteractor:= nil;
  if FDetails <> nil then
  begin
    FDetails.Clear;
    FDetails:= nil;
  end;

  FModel:= nil;
  FView:= nil;
  inherited;
end;

function TPresenter.GetInteractor: IInteractorList;
begin
  Result:= FInteractor;
end;

{ TBooleanTypePresenter }

constructor TBooleanPresenter.Create(const AOwner: IInterface;
  const Model: IModel; const View: IView);
begin
  inherited;
  Interactor.Add(TBooleanInteractor);
end;

{ TObjectListTypePresenter }

constructor TObjectListPresenter.Create(const AOwner: IInterface;
  const Model: IModel; const View: IView);
begin
  inherited;
  // selection
  Interactor.Add(TAddSelectionInteractor);
  Interactor.Add(TRemoveSelectionInteractor);
  Interactor.Add(TClearSelectionInteractor);
  // list operations
  Interactor.Add(TObjectListClearInteractor);
  Interactor.Add(TObjectListAddInteractor);
  Interactor.Add(TObjectListInsertInteractor);
  Interactor.Add(TObjectListDeleteInteractor);
end;

{ TMemberPresenter }

procedure TMemberPresenter.UpdateMasterLink(const Item: IInterface);
var
  LMember: IMemberType;
  LView: IObserver;
begin
  if Supports(View, IObserver, LView) then
  begin
    if Model.ObjectValue <> nil then
      (Model.ObjectValue as IValueType).Subject.Detach(LView);

    if Item <> nil then
    begin
      LMember:= (Item as IObjectType).Member[MasterMember];
      if LMember <> nil then
      begin
        Model.ObjectValue:= LMember;
       (Model.ObjectValue as IValueType).Subject.Attach(LView);
      end;
    end;
  end;
end;

end.

