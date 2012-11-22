unit JazzVCLView;

interface

uses
  Classes,
  ComCtrls,
  Controls,
  ExtCtrls,
  Forms,
  Mask,
  Messages,
//  Spin,
  StdCtrls,
  JazzNotifyIntf,
  JazzValueTypeIntf,
  JazzVCLViewIntf,
  JazzView,
  JazzViewIntf;

type
  TCheckBoxView = class(TBooleanView)
  private
    FCheckBox: TCheckBox;
    FHandled: boolean;
  protected
    procedure HandleClick(Sender: TObject); virtual;
    procedure HandleEnter(Sender: TObject); virtual;
    procedure HandleExit(Sender: TObject); virtual;
    procedure HandleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
  end;

  TRadioButtonView = class(TBooleanView)
  private
    FHandled: boolean;
    FRadioButton: TRadioButton;
    procedure HandleClick(Sender: TObject);
    procedure HandleEnter(Sender: TObject);
    procedure HandleExit(Sender: TObject);
    procedure HandleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
  end;

  TRadioButtonSmallIntView = class(TSmallIntView)
  private
    FCheckedValue: SmallInt;
    FHandled: boolean;
    FRadioButton: TRadioButton;
    procedure HandleClick(Sender: TObject);
    procedure HandleEnter(Sender: TObject);
    procedure HandleExit(Sender: TObject);
    procedure HandleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject; const CheckedValue: SmallInt); reintroduce; virtual;
  end;

  TEditView = class(TStringView)
  private
    FEdit: TEdit;
    FHandled: boolean;
    procedure HandleChange(Sender: TObject);
    procedure HandleEnter(Sender: TObject);
    procedure HandleExit(Sender: TObject);
    procedure HandleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
  end;

  TMaskEditView = class(TStringView)
  private
    FMaskEdit: TMaskEdit;
    FHandled: boolean;
    procedure HandleChange(Sender: TObject);
    procedure HandleEnter(Sender: TObject);
    procedure HandleExit(Sender: TObject);
    procedure HandleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
  end;

  TLabelView = class(TStringView)
  private
    FLabel: TLabel;
  protected
    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
  end;

  TDateTimePickerView = class(TDateView)
  private
    FDateTimeEdit: TDateTimePicker;
    FHandled: boolean;
    function IsDateNull: boolean;
    procedure FormatDateTime(const Hide: boolean = True);

    procedure HandleChange(Sender: TObject);
    procedure HandleEnter(Sender: TObject);
    procedure HandleExit(Sender: TObject);
    procedure HandleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
  end;

  TSpinEditView = class(TIntegerView)
  private
    FHandled: boolean;
//    FLock: boolean;
//    FSpinEdit: TSpinEdit;
  protected
    procedure HandleChange(Sender: TObject);
    procedure HandleEnter(Sender: TObject);
    procedure HandleExit(Sender: TObject);
    procedure HandleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
  end;

  TEditMemoView = class(TMemoView)
  private
    FMemo: TMemo;
    FHandled: boolean;
    procedure HandleChange(Sender: TObject);
    procedure HandleEnter(Sender: TObject);
    procedure HandleExit(Sender: TObject);
    procedure HandleKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
  end;

  TImageView = class(TBlobView)
  private
    FImage: TImage;
    FLoading: boolean;
    procedure HandleChange(Sender: TObject);
  protected
    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
  end;

  TStatusPanelView = class(TStringView)
  private
    FStatusPanel: TStatusPanel;
  protected
    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
  end;

  TFormView = class(TObjectView)
  private
    FForm: TForm;
  protected
    function GetForm: TForm;

    procedure HandleShortCut(var Msg: TWMKey; var Handled: Boolean); virtual;
    procedure HandleEnter(Sender: TObject); virtual;
    procedure HandleExit(Sender: TObject); virtual;

    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
    property Form: TForm read GetForm;
  end;

  TListItemView = class(TMemberView, IListItemView)
  private
    FCaption: string;
    FColumn: TListColumn;
    FMemberName: string;
    FListView: Pointer;
  protected
    function GetCaption: string;
    function GetColumn: TListColumn;
    function GetListView: IListViewView;
    function GetMemberName: string;
    procedure SetCaption(const Value: string);
    procedure SetColumn(const Value: TListColumn);
    procedure SetListView(const Value: IListViewView);
    procedure SetMemberName(const Value: string);

    function GetText(const AObject: IObjectType): string;
    procedure Update(const Notification: IObjectEvent); override;

    property Caption: string read GetCaption write SetCaption;
    property Column: TListColumn read GetColumn write SetColumn;
    property ListView: IListViewView read GetListView write SetListView;
    property MemberName: string read GetMemberName write SetMemberName;
  end;

  TListViewView = class(TObjectListView, IListViewView)
  private
    FListView: TListView;
    FLock: boolean;
    FItems: IInterfaceList;
    function GetCount: Integer;
    function GetItems(Index: Integer): IListItemView;
  protected
    procedure SetControl(const Value: TObject); override;
    procedure Update(const Notification: IObjectEvent); override;

    function ObjectList: IObjectListType;
    procedure HandleData(Sender: TObject; Item: TListItem);
    procedure HandleSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

    function Add(const Column: TObject; const MemberName: string): IListItemView;
    procedure Clear;
    procedure Remove(const Index: Integer);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: IListItemView read GetItems; default;
  public
    constructor Create(const Control: TObject); override;
    destructor Destroy; override;
    procedure Refresh;
  end;

  // TODO: check change IObjectListType to TObject
  PLinkedList = ^TLinkedList;
  TLinkedList = record
    MasterList: IObjectListType;
    DetailList: IObjectListType;
    MasterID: PAnsiChar;
    DetailID: PAnsiChar;
  end;

  TTreeViewView = class(TObjectListView, ITreeViewView)
  private
    FDetailID: Integer;
    FOnCaptionNeeded: TTreeNodeCaptionNeeded;
    FOnChange: TTreeViewOnChangeEvent;
    FTreeView: TTreeView;
    FItems: TList;
    FMasterID: string;
    function GetMasterList: IObjectListType;
    function GetOnCaptionNeeded: TTreeNodeCaptionNeeded;
    function GetOnChange: TTreeViewOnChangeEvent;
    procedure InsertList(const List: IObjectListType); overload;
    procedure InsertList(const AObjectList: IObjectListType; MasterID, DetailID: string); overload;

    procedure PopulateFromLists;
    procedure PopulateFromSingleList;

    procedure SetOnCaptionNeeded(const Value: TTreeNodeCaptionNeeded);
    procedure SetOnChange(const Value: TTreeViewOnChangeEvent);
    procedure AddItem(const AObject: IObjectType; AIndex: Integer; AParent: TTreeNode = nil);
    function GetParentNode(MasterID: string): TTreeNode;
  protected
    procedure SetControl(const Value: TObject); override;
    procedure Update(const Notification: IObjectEvent); override;

    function ObjectList: IObjectListType;
    procedure SetDetailID(Value: Integer);
    procedure SetMasterID(const Value: string);

    function GetDetailID: Integer;
    function GetMasterID: string;
    function ListCount: Integer;
    procedure AddList(const MasterList, DetailList: IObjectListType; MasterID, DetailID: string);
    procedure Clear;
    procedure RemoveList(Index: Integer);
    procedure TreeViewNodeChange(Sender: TObject; Node: TTreeNode);
  public
    constructor Create(const Control: TObject); override;
    destructor Destroy; override;
    procedure Refresh;
    property DetailID: Integer read GetDetailID write SetDetailID;
    property MasterID: string read GetMasterID write SetMasterID;
    property OnChange: TTreeViewOnChangeEvent read GetOnChange write SetOnChange;
    property OnCaptionNeeded: TTreeNodeCaptionNeeded read GetOnCaptionNeeded write SetOnCaptionNeeded;
  end;

type
  //*
  TStaticTextView = class(TStringView)
  private
    FStaticText: TStaticText;
  protected
    procedure Update(const Notification: IObjectEvent); override;
  public
    constructor Create(const Control: TObject); override;
  end;

implementation

uses
  JazzNotify,
  Graphics,
  SysUtils,
  Windows;

{ TCheckBoxView }

procedure TCheckBoxView.HandleClick(Sender: TObject);
begin
  if FHandled then Exit;
  DoChanged(FCheckBox.Checked);
  FHandled:= True;
end;

constructor TCheckBoxView.Create(const Control: TObject);
begin
  inherited;
  FCheckBox:= Control as TCheckBox;
  FCheckBox.OnClick:= HandleClick;
  FCheckBox.OnEnter:= HandleEnter;
  FCheckBox.OnExit:= HandleExit;
  FCheckBox.OnKeyUp:= HandleKeyUp;
  FHandled:= False;
end;

procedure TCheckBoxView.Update(const Notification: IObjectEvent);
var
  LBoolean: IBooleanType;
begin
  inherited;
  if CheckAcceptNotify(Notification) and
    Supports(Notification.Sender, IBooleanType, LBoolean) then
    FCheckBox.Checked:= LBoolean.Value;
end;

procedure TCheckBoxView.HandleEnter(Sender: TObject);
begin
  //TODO; notify 'add.selection'
//  DoAddSelection;
  FHandled:= False;
end;

procedure TCheckBoxView.HandleExit(Sender: TObject);
begin
  //TODO; notify 'remove.selection'
//  DoRemoveSelection;
  HandleClick(Sender);
end;

procedure TCheckBoxView.HandleKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then HandleClick(Sender);
end;

{ TSpinEditView }

constructor TSpinEditView.Create(const Control: TObject);
begin
  inherited;
//  FSpinEdit:= Control as TSpinEdit;
//  FSpinEdit.OnClick:= HandleChange;
//  FSpinEdit.OnEnter:= HandleEnter;
//  FSpinEdit.OnExit := HandleExit;
//  FSpinEdit.OnKeyUp:= HandleKeyUp;
//  FHandled:= False;
end;

procedure TSpinEditView.HandleChange(Sender: TObject);
begin
//  if FHandled then Exit;
//  if FLock then Exit;
//  FLock:= True; 
//  try
//    if FSpinEdit.Text = EmptyStr then FSpinEdit.Value:= 0;
//    DoChanged(FSpinEdit.Value);
//  finally
//    FLock:= False;
//    FHandled:= True;
//  end;
end;

procedure TSpinEditView.HandleEnter(Sender: TObject);
begin
  //TODO; notify 'add.selection'
  FHandled:= False;
end;

procedure TSpinEditView.HandleExit(Sender: TObject);
begin
  //TODO; notify 'remove.selection'
  Self.HandleChange(Sender);
end;

procedure TSpinEditView.HandleKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then Self.HandleChange(Sender);
end;

procedure TSpinEditView.Update(const Notification: IObjectEvent);
//var
//  LInteger: IIntegerType;
begin
  inherited;
  
//  if CheckAcceptNotify(Notification) and
//    Supports(Notification.Sender, IIntegerType, LInteger) then
//    FSpinEdit.Value:= LInteger.Value;
end;

{ TEditView }

constructor TEditView.Create(const Control: TObject);
begin
  inherited;
  FEdit:= Control as TEdit;
  FEdit.OnClick:= HandleChange;
  FEdit.OnEnter:= HandleEnter;
  FEdit.OnExit := HandleExit;
  FEdit.OnKeyUp:= HandleKeyUp;
  FHandled:= False;
end;

procedure TEditView.HandleChange(Sender: TObject);
begin
  if FHandled then Exit;
  DoChanged(FEdit.Text);
  if (ObjectValue as IMemberType).AsString <> FEdit.Text then
    (ObjectValue as IMemberType).AsString:= FEdit.Text;
  FHandled:= True; 
end;

procedure TEditView.HandleEnter(Sender: TObject);
begin
  //TODO; notify 'add.selection'
  FHandled:= False;  
end;

procedure TEditView.HandleExit(Sender: TObject);
begin
  //TODO; notify 'remove.selection'
  Self.HandleChange(Sender);  
end;

procedure TEditView.HandleKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then Self.HandleChange(Sender);
end;

procedure TEditView.Update(const Notification: IObjectEvent);
var
  LMember: IMemberType;
  LString: IStringType;
begin
  inherited;
  
  if CheckAcceptNotify(Notification) then
  begin
    if Supports(Notification.Sender, IStringType, LString) then
      FEdit.Text:= LString.Value
    else
    if Supports(Notification.Sender, IMemberType, LMember) then
      FEdit.Text:= LMember.AsString
    else
      FEdit.Clear;
  end;
end;

{ TRadioButtonView }

constructor TRadioButtonView.Create(const Control: TObject);
begin
  inherited;
  FRadioButton:= Control as TRadioButton;
  FRadioButton.OnClick:= HandleClick;
  FRadioButton.OnEnter:= HandleEnter;
  FRadioButton.OnExit:= HandleExit;
  FRadioButton.OnKeyUp:= HandleKeyUp;
  FHandled:= False;
end;

procedure TRadioButtonView.HandleClick(Sender: TObject);
begin
  if FHandled then Exit;
  DoChanged(FRadioButton.Checked);
  FHandled:= True;
end;

procedure TRadioButtonView.HandleEnter(Sender: TObject);
begin
  //TODO; notify 'add.selection'
  FHandled:= False;  
end;

procedure TRadioButtonView.HandleExit(Sender: TObject);
begin
  //TODO; notify 'remove.selection'
  HandleClick(Sender);
end;

procedure TRadioButtonView.HandleKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then HandleClick(Sender);
end;

procedure TRadioButtonView.Update(const Notification: IObjectEvent);
var
  LBoolean: IBooleanType;
begin
  inherited;
  if CheckAcceptNotify(Notification) and
    Supports(Notification.Sender, IBooleanType, LBoolean) then
    FRadioButton.Checked:= LBoolean.Value;
end;

{ TRadioButtonSmallIntView }

constructor TRadioButtonSmallIntView.Create(const Control: TObject;
  const CheckedValue: SmallInt);
begin
  inherited Create(Control);

  FCheckedValue:= CheckedValue;
  FRadioButton:= Control as TRadioButton;
  FRadioButton.OnClick:= HandleClick;
  FRadioButton.OnEnter:= HandleEnter;
  FRadioButton.OnExit:= HandleExit;
  FRadioButton.OnKeyUp:= HandleKeyUp;
  FHandled:= False;
end;

procedure TRadioButtonSmallIntView.HandleClick(Sender: TObject);
begin
  if FHandled then Exit;
  DoChanged(FCheckedValue);
  FHandled:= True;
end;

procedure TRadioButtonSmallIntView.HandleEnter(Sender: TObject);
begin
  //TODO; notify 'add.selection'
  FHandled:= False;  
end;

procedure TRadioButtonSmallIntView.HandleExit(Sender: TObject);
begin
  //TODO; notify 'remove.selection'
  HandleClick(Sender);
end;

procedure TRadioButtonSmallIntView.HandleKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then HandleClick(Sender);
end;

procedure TRadioButtonSmallIntView.Update(const Notification: IObjectEvent);
var
  LSmallInt: ISmallIntType;
begin
  inherited;
  if CheckAcceptNotify(Notification) and
    Supports(Notification.Sender, ISmallIntType, LSmallInt) then
    FRadioButton.Checked:= LSmallInt.Value = FCheckedValue; 
end;

{ TMemoView }

constructor TEditMemoView.Create(const Control: TObject);
begin
  inherited;
  FMemo:= Control as TMemo;
  FMemo.OnClick:= HandleChange;
  FMemo.OnEnter:= HandleEnter;
  FMemo.OnExit := HandleExit;
  FMemo.OnKeyUp:= HandleKeyUp;
  FHandled:= False;
end;

procedure TEditMemoView.HandleChange(Sender: TObject);
begin
  if FHandled or (ObjectValue = nil) then Exit;
  DoChanged(FMemo.Lines);
  if (ObjectValue as IMemberType).AsString <> FMemo.Lines.Text then
  begin
    (ObjectValue as IMemberType).AsString:= FMemo.Lines.Text;
    FHandled:= True;
  end;
end;

procedure TEditMemoView.HandleEnter(Sender: TObject);
begin
  //TODO; notify 'add.selection'
  FHandled:= False;
end;

procedure TEditMemoView.HandleExit(Sender: TObject);
begin
  //TODO; notify 'remove.selection'
  HandleChange(Sender);  
end;

procedure TEditMemoView.HandleKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (ssCtrl in Shift) then Self.HandleChange(Sender);
end;

procedure TEditMemoView.Update(const Notification: IObjectEvent);
var
  LMemo: IMemoType;
begin
  inherited;
  if CheckAcceptNotify(Notification) and
    Supports(Notification.Sender, IMemoType, LMemo) then
    FMemo.Lines:= LMemo.Value;
end;

{ TStatusPanelView }

constructor TStatusPanelView.Create(const Control: TObject);
begin
  inherited;
  FStatusPanel:= Control as TStatusPanel;
end;

procedure TStatusPanelView.Update(const Notification: IObjectEvent);
var
  LString: IStringType;
begin
  inherited;
  
  if CheckAcceptNotify(Notification) and
    Supports(Notification.Sender, IStringType, LString) then
    FStatusPanel.Text:= LString.Value;
end;

{ TFormView }

constructor TFormView.Create(const Control: TObject);
begin
  inherited;
  FForm:= Control as TForm;
  FForm.OnActivate:= HandleEnter;
  FForm.OnDeactivate:= HandleExit;
  FForm.OnShortCut:= HandleShortCut;
end;

function TFormView.GetForm: TForm;
begin
  Result:= FForm;
end;

procedure TFormView.HandleEnter(Sender: TObject);
begin

end;

procedure TFormView.HandleExit(Sender: TObject);
begin

end;

procedure TFormView.HandleShortCut(var Msg: TWMKey; var Handled: Boolean);
begin

end;

procedure TFormView.Update(const Notification: IObjectEvent);
begin
  inherited;

end;

{ TListItemView }

function TListItemView.GetCaption: string;
begin
  Result:= FCaption;
end;

function TListItemView.GetColumn: TListColumn;
begin
  Result:= FColumn;
end;

function TListItemView.GetListView: IListViewView;
begin
  Result:= IListViewView(FListView);
end;

function TListItemView.GetMemberName: string;
begin
  Result:= FMemberName;
end;

function TListItemView.GetText(const AObject: IObjectType): string;
begin
  // TODO: format text -> Display Mask
  Result:= AObject.Member[MemberName].AsString;
end;

procedure TListItemView.SetCaption(const Value: string);
begin
  FCaption:= Value;
end;

procedure TListItemView.SetColumn(const Value: TListColumn);
begin
  FColumn:= Value;
end;

procedure TListItemView.SetListView(const Value: IListViewView);
begin
  FListView:= Pointer(Value);
end;

procedure TListItemView.SetMemberName(const Value: string);
begin
  FMemberName:= Value;
end;

procedure TListItemView.Update(const Notification: IObjectEvent);
begin
  inherited;
  if ListView <> nil then ListView.Refresh; 
end;

destructor TListViewView.Destroy;
begin
  Clear;
  FItems:= nil;
  inherited;
end;

{ TListViewView }

function TListViewView.Add(const Column: TObject; const MemberName: string):
  IListItemView;
begin
  Result:= TListItemView.Create(Column);
  Result.MemberName:= MemberName;
  Result.ListView:= Self;
  FItems.Add(Result);
end;

procedure TListViewView.Clear;
begin
  if FItems <> nil then FItems.Clear;
  FItems:= nil;
end;

function TListViewView.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

function TListViewView.GetItems(Index: Integer): IListItemView;
begin
  Result:= FItems[Index] as IListItemView;
end;

procedure TListViewView.HandleData(Sender: TObject; Item: TListItem);
var
  I: Integer;
  LObject: IObjectType;
begin
  if ObjectList <> nil then
  begin
    if Item.Data = nil then
    begin
      LObject:= ObjectList.Items[Item.Index];
      Item.Data:= Pointer(LObject);
    end
    else
      LObject:= IObjectType(Item.Data);

    if LObject <> nil then
    begin
      Item.SubItems.Clear;
      Item.Caption:= Items[0].GetText(LObject);
      for I:= 1 to Count -1 do Item.SubItems.Add(Items[I].GetText(LObject))
    end;
  end;
end;

procedure TListViewView.HandleSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if (Item <> nil) and (ObjectList <> nil) then
    DoAddSelection(ObjectList[Item.Index]);
end;

function TListViewView.ObjectList: IObjectListType;
begin
  Result:= ObjectValue as IObjectListType;
end;

procedure TListViewView.Refresh;
begin
  if FLock then Exit;
  FLock:= True;
  try
    if (FListView <> nil) and (ObjectList <> nil) then
    begin
      if FListView.Items.Count <> ObjectList.Count then
      begin
        FListView.Items.Count:= ObjectList.Count;
        FListView.Repaint;
      end;
    end;
  finally
    FLock:= False; 
  end;
end;

procedure TListViewView.Remove(const Index: Integer);
begin
  if Count > Index then FItems.Delete(Index);
end;

procedure TListViewView.SetControl(const Value: TObject);
const
  SAllocBy = 32;
begin
  if (Control <> nil) and (Value = nil) then
  begin
    FListView.OnData:= nil;
    FListView.OnSelectItem:= nil;
  end;

  inherited;

  if Control <> nil then
  begin
    FListView:= Control as TListView;
    FListView.AllocBy  := SAllocBy;
    FListView.OwnerData:= True;
    FListView.RowSelect:= True;
    FListView.ViewStyle:= vsReport;

    FListView.OnData:= HandleData;
    FListView.OnSelectItem:= HandleSelectItem;
  end;
end;

procedure TListViewView.Update(const Notification: IObjectEvent);
begin
  inherited;
  case Notification.NotifyType of
    ntDetached:
      begin
        FListView.Selected:= nil;
        FListView.Clear;
      end;
    ntDeleting:
      FListView.Selected:= nil;
  else
    Refresh;
  end;
end;

constructor TListViewView.Create(const Control: TObject);
begin
  inherited;
  FItems:= TInterfaceList.Create;
end;

{ TDateTimePickerView }

procedure TDateTimePickerView.FormatDateTime(const Hide: boolean);
begin
  if Hide then FDateTimeEdit.Format:= ' ' else FDateTimeEdit.Format:= '';
end;

constructor TDateTimePickerView.Create(const Control: TObject);
begin
  inherited;
  FDateTimeEdit:= Control as TDateTimePicker;
  FDateTimeEdit.OnClick:= HandleChange;
  FDateTimeEdit.OnEnter:= HandleEnter;
  FDateTimeEdit.OnExit := HandleExit;
  FDateTimeEdit.OnKeyUp:= HandleKeyUp;
  FHandled:= False;
end;

procedure TDateTimePickerView.HandleChange(Sender: TObject);
begin
  if FHandled then Exit;
  DoChanged(FDateTimeEdit.DateTime);
  if (ObjectValue as IDateType).Value <> FDateTimeEdit.DateTime then
   (ObjectValue as IDateType).Value:= FDateTimeEdit.DateTime;
  FHandled:= True; 
end;

procedure TDateTimePickerView.HandleEnter(Sender: TObject);
begin
  //TODO; notify 'add.selection'
  FormatDateTime(False);
  FHandled:= False;
end;

procedure TDateTimePickerView.HandleExit(Sender: TObject);
begin
  //TODO; notify 'remove.selection'
  if not IsDateNull then FormatDateTime;
  Self.HandleChange(Sender);
end;

function TDateTimePickerView.IsDateNull: boolean;
begin
  Result:= Trunc(FDateTimeEdit.Date) = 0;
end;

procedure TDateTimePickerView.Update(const Notification: IObjectEvent);
var
  LDate: IDateType;
begin
  inherited;
  if CheckAcceptNotify(Notification) then
  begin
    if Supports(Notification.Sender, IDateType, LDate) and not LDate.IsNull then
    begin
      FormatDateTime(False);
      FDateTimeEdit.DateTime:= LDate.Value;
    end
    else
      FormatDateTime;
  end;
end;

procedure TDateTimePickerView.HandleKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then Self.HandleChange(Sender);
end;

{ TMaskEditView }

constructor TMaskEditView.Create(const Control: TObject);
begin
  inherited;
  FMaskEdit:= Control as TMaskEdit;
  FMaskEdit.OnClick:= HandleChange;
  FMaskEdit.OnEnter:= HandleEnter;
  FMaskEdit.OnExit := HandleExit;
  FMaskEdit.OnKeyUp:= HandleKeyUp;
  FHandled:= False;
end;

procedure TMaskEditView.HandleChange(Sender: TObject);
begin
  if FHandled then Exit;
  DoChanged(FMaskEdit.Text);
  if (ObjectValue as IMemberType).AsString <> FMaskEdit.Text then
    (ObjectValue as IMemberType).AsString:= FMaskEdit.Text;
  FHandled:= True; 
end;

procedure TMaskEditView.HandleEnter(Sender: TObject);
begin
  //TODO; notify 'add.selection'
  FHandled:= False;  
end;

procedure TMaskEditView.HandleExit(Sender: TObject);
begin
  //TODO; notify 'remove.selection'
  Self.HandleChange(Sender);  
end;

procedure TMaskEditView.HandleKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then Self.HandleChange(Sender);
end;

procedure TMaskEditView.Update(const Notification: IObjectEvent);
var
  LString: IStringType;
begin
  inherited;
  
  if CheckAcceptNotify(Notification) then
  begin
    if Supports(Notification.Sender, IStringType, LString) then
      FMaskEdit.Text:= LString.Value
    else
      FMaskEdit.Clear;
  end;
end;

{ TLabelView }

constructor TLabelView.Create(const Control: TObject);
begin
  inherited;
  FLabel:= Control as TLabel;
end;

procedure TLabelView.Update(const Notification: IObjectEvent);
var
  LMember: IMemberType;
begin
  inherited;
  
  if CheckAcceptNotify(Notification) then
  begin
    if Supports(Notification.Sender, IMemberType, LMember) then
      FLabel.Caption:= LMember.AsString
    else
      FLabel.Caption:= '';
  end;
end;

{ TTreeViewView }

procedure TTreeViewView.AddList(const MasterList, DetailList: IObjectListType;
    MasterID, DetailID: string);
var
  LItem: PLinkedList;
begin
  // TODO: Attach list - make notification active
  New(LItem);
  LItem.MasterList:= MasterList;
  LItem.DetailList:= DetailList;
  LItem.MasterID:=  PAnsiChar(MasterID);
  LItem.DetailID:= PAnsiChar(DetailID);
  FItems.Add(LItem);
  Refresh;
end;

procedure TTreeViewView.Clear;
var
  I: Integer;
begin
  for I := FItems.Count -1 downto 0 do
    RemoveList(I);
end;

constructor TTreeViewView.Create(const Control: TObject);
begin
  inherited;
  FItems:= TList.Create;
end;

destructor TTreeViewView.Destroy;
begin
  Clear;
  FItems.Free;
  inherited;
end;

function TTreeViewView.GetDetailID: Integer;
begin
  Result := FDetailID;
end;

function TTreeViewView.GetMasterID: string;
begin
  Result := FMasterID;
end;

function TTreeViewView.GetOnCaptionNeeded: TTreeNodeCaptionNeeded;
begin
  Result:= FOnCaptionNeeded;
end;

function TTreeViewView.GetOnChange: TTreeViewOnChangeEvent;
begin
  Result:= FOnChange;
end;

function TTreeViewView.ListCount: Integer;
begin
  Result:= FItems.Count;
end;

function TTreeViewView.ObjectList: IObjectListType;
begin
  Result:= ObjectValue as IObjectListType;
end;

function TTreeViewView.GetMasterList: IObjectListType;
var
  I: Integer;
  LItem: PLinkedList;
begin
  Result:= nil;
   for I := 0 to FItems.Count - 1 do
   begin
     LItem:= PLinkedList(FItems[I]);

     if LItem.MasterList = nil then
     begin
       Result:= LItem.DetailList;
       Break;
     end;
   end;
end;

function TTreeViewView.GetParentNode(MasterID: string): TTreeNode;
var
  I: Integer;
  LObject: IObjectType;
begin
  Result:= nil;
  for I := 0 to FTreeView.Items.Count - 1 do
  begin
    Pointer(LObject):= FTreeView.Items[I].Data;
    if LObject.Member[MasterID].AsString = MasterID then
    begin
      Result:= FTreeView.Items[I];
      Break;
    end;
  end;
end;

procedure TTreeViewView.InsertList(const AObjectList: IObjectListType;
    MasterID, DetailID: string);
var
  I: Integer;
begin
  for I := 0 to AObjectList.Count - 1 do
    AddItem(AObjectList[I], I, GetParentNode(MasterID));
end;

procedure TTreeViewView.AddItem(const AObject: IObjectType; AIndex: Integer;
    AParent: TTreeNode = nil);
var
  LNode: TTreeNode;
  LCaption: string;
  LImageIndex: Integer;
begin
  LCaption:= EmptyStr;
  LImageIndex:= -1;

  if Assigned(FOnCaptionNeeded) then
    FOnCaptionNeeded(FTreeView, AObject, LCaption, LImageIndex);

  if LCaption = EmptyStr then
    LCaption:= AObject.Members[0].AsString;
    
  LNode:= FTreeView.Items.AddObject(AParent, LCaption, TObject(AObject));
  LNode.ImageIndex:= LImageIndex;
  LNode.SelectedIndex:= LImageIndex;
  LNode.StateIndex:= LImageIndex;
end;

procedure TTreeViewView.InsertList(const List: IObjectListType);
var
  I, J: Integer;
  LItem: PLinkedList;
  LMaster: IObjectListType;
begin
  Assert(FTreeView.Items.Count = 0, 'Master list already populated');
  for I:= 0 to List.Count - 1 do AddItem(List[I], I);

  for I := 0 to FItems.Count -1 do
  begin
    LMaster:= PLinkedList(FItems[I]).MasterList;
    if LMaster = nil then Continue;
    for J := 0 to FItems.Count -1 do
    begin
      LItem:= PLinkedList(FItems[J]);
      if LItem.MasterList = LMaster then
        InsertList(LItem.DetailList, LItem.MasterID, LItem.DetailID);
    end;
  end;
end;

procedure TTreeViewView.PopulateFromLists;
begin
  with FTreeView do
  begin
    Items.BeginUpdate;
    try
      Items.Clear;
      InsertList(GetMasterList);
    finally
      Items.EndUpdate;
    end;
  end;
end;

procedure TTreeViewView.PopulateFromSingleList;
begin
  // TODO: Populate only using ObjectValue as ObjectList
end;

procedure TTreeViewView.Refresh;
begin
  if (FTreeView = nil) then Exit;

  if ListCount = 0 then
    PopulateFromSingleList
  else
    PopulateFromLists;
end;

procedure TTreeViewView.RemoveList(Index: Integer);
var
  LItem: PLinkedList;
begin
  LItem:= PLinkedList(FItems[Index]);
  Finalize(LItem^);
  Dispose(FItems[Index]);
  FItems.Delete(Index);
end;

procedure TTreeViewView.SetControl(const Value: TObject);
begin
  if (Control <> nil) and (Value = nil) then
  begin
    FTreeView.OnChange:= nil;
    FTreeView:= nil;
  end;

  inherited;

  if Control <> nil then
  begin
    FTreeView:= Control as TTreeView;
    FTreeView.OnChange:= TreeViewNodeChange;
  end;
end;

procedure TTreeViewView.SetDetailID(Value: Integer);
begin
  if FDetailID <> Value then
  begin
    FDetailID := Value;
  end;
end;

procedure TTreeViewView.SetMasterID(const Value: string);
begin
  if FMasterID <> Value then
  begin
    FMasterID := Value;
  end;
end;

procedure TTreeViewView.SetOnCaptionNeeded(const Value: TTreeNodeCaptionNeeded);
begin
  FOnCaptionNeeded:= Value;
end;

procedure TTreeViewView.SetOnChange(const Value: TTreeViewOnChangeEvent);
begin
  FOnChange:= Value;
end;

procedure TTreeViewView.TreeViewNodeChange(Sender: TObject; Node: TTreeNode);
var
  LSelection: IObjectType;
begin
  if Assigned(FOnChange) then
  begin
    LSelection:= IObjectType(Node.Data);
    FOnChange(FTreeView, Self, LSelection);
  end;
end;

procedure TTreeViewView.Update(const Notification: IObjectEvent);
begin
  inherited;
  case Notification.NotifyType of
    ntAttached:
      begin
        AddList(nil, ObjectList, '', '');
        Refresh;      
      end;
    ntDetached: FTreeView.Items.Clear;
  else
    Refresh;
  end;
end;

{ TImageView }

constructor TImageView.Create(const Control: TObject);
begin
  inherited;
  FImage:= Control as TImage;
  FImage.Picture.OnChange:= HandleChange;
end;

procedure TImageView.HandleChange(Sender: TObject);
var
  LStream: TMemoryStream;
begin
  if FLoading then Exit;
  FLoading:= True;
  try
    LStream:= TMemoryStream.Create;
    try
      FImage.Picture.BitMap.SaveToStream(LStream);
      if LStream.Size <> (ObjectValue as IBlobType).Value.Size then
      begin
        LStream.Clear;
        FImage.Picture.Graphic.SaveToStream(LStream);
        (ObjectValue as IBlobType).Value:= LStream;
        DoChanged(LStream);
      end;
    finally
      LStream.Free;
    end;
  finally
    FLoading:= False;
  end;
end;

procedure TImageView.Update(const Notification: IObjectEvent);
var
  LImage: IBlobType;
begin
  inherited;
  if FLoading then Exit;
  FLoading:= True;
  try
    if CheckAcceptNotify(Notification) and
      Supports(Notification.Sender, IBlobType, LImage) then
    begin
      if LImage.Value.Size <> 0 then
        FImage.Picture.Bitmap.LoadFromStream(LImage.Value)
      else
        FImage.Picture.Graphic:= nil;
      FImage.Repaint;
    end;
  finally
    FLoading:= False; 
  end;
end;

{ TStaticTextView }

constructor TStaticTextView.Create(const Control: TObject);
begin
  inherited;
  FStaticText:= Control as TStaticText;
end;

procedure TStaticTextView.Update(const Notification: IObjectEvent);
var
  LMember: IMemberType;
begin
  inherited;
  if CheckAcceptNotify(Notification) then
  begin
    if Supports(Notification.Sender, IMemberType, LMember) then
      FStaticText.Caption := LMember.AsString
    else
      FStaticText.Caption := '';
  end;
end;


end.

