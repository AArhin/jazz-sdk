unit ViewEditores;


interface

uses Classes, StdCtrls, ComCtrls,
  JazzObserverIntf, JazzValueTypeIntf, JazzTypes, JazzNotifyIntf;

type
  IAutoEdit = interface(IInterface)
    ['{78FDD520-4CCC-4D1B-BCD0-40C1F0A0EEBF}']
    function GetMember: IMemberType;
    function GetEdit: TEdit;
    property Edit: TEdit read GetEdit;
    property Member: IMemberType read GetMember;
  end;
  
  TAutoEdit = class(TInterfacedObject, IAutoEdit, IObserver)
  private
    FEdit: TEdit;
    FMember: Pointer;
    function GetEdit: TEdit;
    function GetMember: IMemberType;
    procedure EditChange(Sender: TObject);
  public
    constructor Create(const Member: IMemberType; const AEdit: TEdit); reintroduce; virtual;
    destructor Destroy; override;
    procedure Update(const Notification: IObjectEvent);
    property Edit: TEdit read GetEdit;
    property Member: IMemberType read GetMember;
  end;

  IStatusBarObserver = interface(IInterface)
    ['{B17129F1-18BD-4E2C-ADA9-98CC5CD7AA57}']
    function GetObjectList: IObjectListType;
    function GetStatusBar: TStatusBar;
    procedure Finalize;
    procedure SetObjectList(const Value: IObjectListType);
    procedure SetStatusBar(const Value: TStatusBar);
    property ObjectList: IObjectListType read GetObjectList write SetObjectList;
    property StatusBar: TStatusBar read GetStatusBar write SetStatusBar;
  end;

  TStatusBarObserver = class(TInterfacedObject, IStatusBarObserver, IObserver)
  private
    FObjectList: Pointer;
    FStatusBar: TStatusBar;
  protected
    function GetObjectList: IObjectListType;
    function GetStatusBar: TStatusBar;
    procedure SetObjectList(const Value: IObjectListType);
    procedure SetStatusBar(const Value: TStatusBar);

    procedure Finalize;
    procedure Update(const Notification: IObjectEvent);

    property ObjectList: IObjectListType read GetObjectList write SetObjectList;
    property StatusBar: TStatusBar read GetStatusBar write SetStatusBar;
  public
    constructor Create(const ObjectList: IObjectListType; const StatusBar: TStatusBar);
    destructor Destroy; override;
  end;

  IListViewColumn = interface(IInterface)
    ['{63D9DCB8-E41C-4AEB-8657-1ECB034C3792}']
    function GetCaption: string;
    function GetMemberName: string;
    function GetWidth: Integer;
    procedure SetCaption(const Value: string);
    procedure SetMemberName(const Value: string);
    procedure SetWidth(const Value: Integer);
    property Caption: string read GetCaption write SetCaption;
    property MemberName: string read GetMemberName write SetMemberName;
    property Width: Integer read GetWidth write SetWidth;
  end;

  IListViewObserver = interface(IInterface)
    ['{CBCC7CFD-C9F3-40DC-BD6B-606C0DB140DA}']
    function Add(const MemberName, Caption: string; const Width: Integer): IListViewColumn;
    function AddDetail(const DetailObserver: IListViewObserver): Integer;
    function Count: Integer;
    function GetColumns(const Index: Integer): IListViewColumn;
    function GetListView: TListView;
    function GetMasterLinkName: string;
    function GetSelection: IObjectType;
    procedure Clear;
    procedure CreateColumns(const Column: IListViewColumn = nil);
    procedure SetMasterLink(const Master: IListViewObserver; const MasterMember: string);
    procedure SetObjectList(const ObjectList: IObjectListType);

    procedure ListViewData(Sender: TObject; Item: TListItem);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);

    property Columns[const Index: Integer]: IListViewColumn read GetColumns; default;
    property MasterLinkName: string read GetMasterLinkName;
    property Selection: IObjectType read GetSelection;
  end;

  TListViewColumn = class(TInterfacedObject, IListViewColumn, IObserver)
  private
    FCaption: string;
    FMemberName: string;
    FWidth: Integer;
    FColumn: TListColumn;
  protected
    function GetCaption: string;
    function GetMemberName: string;
    function GetWidth: Integer;
    procedure SetCaption(const Value: string);
    procedure SetMemberName(const Value: string);
    procedure SetWidth(const Value: Integer);
    procedure Update(const Notification: IObjectEvent);

    property Caption: string read GetCaption write SetCaption;
    property MemberName: string read GetMemberName write SetMemberName;
    property Width: Integer read GetWidth write SetWidth;
  public
    constructor Create(const MemberName, Caption: string; const Width: Integer); virtual;
  end;

  TListViewObserver = class(TInterfacedObject, IListViewObserver, IObserver)
  private
    FListViewOK: boolean;
    FColumns: IInterfaceList;
    FColumnsAligned: boolean;
    FDetails: TList;
    FList: Pointer;
    FListView: TListView;
    FMaster: Pointer;
    FMasterLinkName: string;

    procedure AlignColumns(const Item: IObjectType);
  protected
    function Add(const MemberName, Caption: string; const Width: Integer): IListViewColumn;
    function AddDetail(const DetailObserver: IListViewObserver): Integer;
    function Count: Integer;
    function GetColumns(const Index: Integer): IListViewColumn;
    function GetListView: TListView;
    function GetMasterLinkName: string;
    function GetSelection: IObjectType;
    procedure Clear;

    procedure CreateColumns(const Column: IListViewColumn = nil);
    procedure SetMasterLink(const Master: IListViewObserver; const MasterMember: string);
    procedure SetObjectList(const ObjectList: IObjectListType);
    procedure Update(const Notification: IObjectEvent);

    procedure ListViewData(Sender: TObject; Item: TListItem);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    
    property Columns[const Index: Integer]: IListViewColumn read GetColumns; default;
    property MasterLinkName: string read GetMasterLinkName;
    property Selection: IObjectType read GetSelection;
  public
    constructor Create(const ObjectList: IObjectListType; const ListView: TListView); virtual;
    destructor Destroy; override;
  end;

implementation

uses JazzSubjectIntf, JazzConsts, ObjectLinkManager, AppConsts,
//  DbugIntf, JazzIntfUtils,
  SysUtils, Dialogs;

{ TAutoEdit }

constructor TAutoEdit.Create(const Member: IMemberType; const AEdit: TEdit);
begin
   inherited Create;
   FMember:= Pointer(Member);
   FEdit:= AEdit;
   FEdit.OnChange:= EditChange;
   (Member as ISubject).Attach(Self);
end;

destructor TAutoEdit.Destroy;
begin
  FEdit.OnChange:= nil;
  FEdit:= nil;
  FMember:= nil;
  inherited;
end;

procedure TAutoEdit.EditChange(Sender: TObject);
var
  LString: IStringType;
begin
  if Supports(Member, IStringType, LString)then LString.Value:= FEdit.Text;
end;

function TAutoEdit.GetEdit: TEdit;
begin
  Result:= FEdit; 
end;

function TAutoEdit.GetMember: IMemberType;
begin
  Result:= IMemberType(FMember);
end;

procedure TAutoEdit.Update(const Notification: IObjectEvent);
var
  LMember: IMemberType;
begin
  if not (Notification.NotifyType = ntDetached) then
  begin
    if Supports(Notification.Sender, IMemberType, LMember) then
      Edit.Text:= LMember.AsString;
  end;
end;

{ TStatusBarObserver }

constructor TStatusBarObserver.Create(const ObjectList: IObjectListType;
  const StatusBar: TStatusBar);
begin
  inherited Create;
  Self.ObjectList:= ObjectList;
  Self.StatusBar := StatusBar ;
  (ObjectList as ISubject).Attach(Self);
end;

destructor TStatusBarObserver.Destroy;
begin
  inherited;
  Finalize;
end;

procedure TStatusBarObserver.Finalize;
begin
  FObjectList:= nil;
  FStatusBar:= nil;
end;

function TStatusBarObserver.GetObjectList: IObjectListType;
begin
  Result:= IObjectListType(FObjectList);
end;

function TStatusBarObserver.GetStatusBar: TStatusBar;
begin
  Result:= FStatusBar;
end;

procedure TStatusBarObserver.SetObjectList(const Value: IObjectListType);
begin
  FObjectList:= Pointer(Value);
end;

procedure TStatusBarObserver.SetStatusBar(const Value: TStatusBar);
begin
  FStatusBar:= Value;

  if FStatusBar <> nil then
  begin
    if FStatusBar.Panels.Count = 0 then
    begin
      FStatusBar.SimplePanel:= False;
      with FStatusBar.Panels.Add do
      begin
        Width := 150;
      end;
    end;
  end;
end;

procedure TStatusBarObserver.Update(const Notification: IObjectEvent);
begin
  if (StatusBar = nil) or (ObjectList = nil) then Exit;

  if StatusBar.Panels.Count = 0 then
  begin
    if Notification.NotifyType = ntDetached then
      StatusBar.SimpleText:= EmptyStr
    else
    if not ObjectList.IsUpdating then
    begin
      if ObjectList.Count = 0 then
        StatusBar.SimpleText:= ''
      else
        StatusBar.SimpleText:= Format('%d ítens', [ObjectList.Count]);
    end;
  end
  else
  begin
    if Notification.NotifyType = ntDetached then
      StatusBar.Panels[0].Text:= EmptyStr
    else
    if not ObjectList.IsUpdating then
    begin
      if ObjectList.Count = 0 then
        StatusBar.Panels[0].Text:= ''
      else
        StatusBar.Panels[0].Text:= Format('%d ítens', [ObjectList.Count]);
    end;
  end;
end;

{ TListViewObserver }

function TListViewObserver.Add(const MemberName, Caption: string;
  const Width: Integer): IListViewColumn;
begin
  Result:= TListViewColumn.Create(MemberName, Caption, Width);
  FColumns.Add(Result);
  CreateColumns(Result); 
end;

function TListViewObserver.AddDetail(
  const DetailObserver: IListViewObserver): Integer;
var
  LDetail: Pointer;
begin
  if FDetails = nil then
  begin
    FDetails:= TList.Create;
    FListView.OnSelectItem:= ListViewSelectItem;
  end;
  LDetail:= Pointer(DetailObserver);
  Result:= FDetails.IndexOf(LDetail);
  
  if Result = NotFound then Result:= FDetails.Add(LDetail);
end;

procedure TListViewObserver.AlignColumns(const Item: IObjectType);
var
  I: Integer;
  LMember: IMemberType;
begin
  if Item = nil then Exit;
  FColumnsAligned:= True;

  FListView.Columns.BeginUpdate;
  try
    for I:= 0 to Count -1 do
    begin
      LMember:= Item.Member[Columns[I].MemberName];

      if (LMember <> nil) and (LMember.ValueTypeKind in
        [vtInteger, vtCurrency, vtFloat, vtLongInt, vtSmallInt]) then
        FListView.Columns[I].Alignment:= taRightJustify;
    end;
  finally
    FListView.Columns.EndUpdate;
    FListView.Repaint;
  end;
end;

procedure TListViewObserver.Clear;
begin
  FColumns.Clear;
  FListView.Columns.Clear;
end;

function TListViewObserver.Count: Integer;
begin
  Result:= FColumns.Count; 
end;

constructor TListViewObserver.Create(const ObjectList: IObjectListType;
  const ListView: TListView);
begin
  inherited Create;
  FListViewOK:= False;
  FColumnsAligned:= False;
  FColumns:= TInterfaceList.Create;
  FListView:= ListView;

  SetObjectList(ObjectList);
end;

procedure TListViewObserver.CreateColumns(const Column: IListViewColumn = nil);
var
  I: Integer;
  LColumn: TListColumn;
begin
  if FListView = nil then Exit;

  if not FListViewOK then
  begin
    FListViewOK:= True;
    FListView.AllocBy  := 32;
    FListView.OwnerData:= True;
    FListView.ReadOnly := True;
    FListView.RowSelect:= True;
    FListView.ViewStyle:= vsReport;
    FListView.Font.Name:= 'Courier New';
  end;

  if Column <> nil then
  begin
    LColumn:= FListView.Columns.Add;
    LColumn.Caption:= Column.Caption;
    LColumn.Width  := Column.Width;
  end
  else
  begin
    FListView.Columns.Clear;
    for I:= 0 to Count -1 do
    begin
      LColumn:= FListView.Columns.Add;
      LColumn.Caption:= Columns[I].Caption;
      LColumn.Width  := Columns[I].Width;
    end;
  end;
end;

destructor TListViewObserver.Destroy;
begin
  if FListView <> nil then FListView.OnSelectItem:= nil;
  if FDetails <> nil then FDetails.Free;
  inherited;
end;

function TListViewObserver.GetColumns(
  const Index: Integer): IListViewColumn;
begin
  Result:= FColumns[Index] as IListViewColumn;
end;

function TListViewObserver.GetListView: TListView;
begin
  Result:= FListView;
end;

function TListViewObserver.GetMasterLinkName: string;
begin
  Result:= FMasterLinkName;
end;

procedure TListViewObserver.ListViewData(Sender: TObject; Item: TListItem);
  function TranslateBoolStr(const Value: string; const YesNo: boolean = True): string;
  begin
    if YesNo then
    begin
      if Value = 'True' then // do not localize
        Result:= S_Yes
      else
        Result:= S_No;
    end
    else
    begin
      if Value = 'True' then // do not localize
        Result:= S_True
      else
        Result:= S_False;
    end;
  end;
  
var
  I: Integer;
  LObject: IObjectType;
  LMember: IMemberType;
begin
  if (FList = nil) or
     (IObjectListType(FList).Count = 0) or
     (FColumns.Count = 0) then
    Exit;

  if Item.Data <> nil then
    LObject:= IObjectType(Item.Data)
  else
  begin
    LObject:= IObjectListType(FList).Items[Item.Index];
    Item.Data:= Pointer(LObject);
  end;

  if LObject = nil then Exit;
  if not FColumnsAligned then AlignColumns(LObject);
  Item.Caption:= LObject.Member[Columns[0].MemberName].AsString;

  if (FColumns.Count > 1) then
  begin
    Item.SubItems.Clear;

    for I:= 1 to FColumns.Count -1 do
    begin
      if Columns[I].MemberName = 'Loaded' then
        Item.SubItems.Add(BoolToStr((LObject as IObjectState).State.Loaded, True))
      else
      if Columns[I].MemberName = 'Persisted' then
        Item.SubItems.Add(BoolToStr((LObject as IObjectState).State.Persisted, True))
      else
      if Columns[I].MemberName = 'Modified' then
        Item.SubItems.Add(BoolToStr((LObject as IObjectState).State.Modified, True))
      else
      if Columns[I].MemberName = 'Deleting' then
        Item.SubItems.Add(BoolToStr((LObject as IObjectState).State.Deleting, True))
      else
      if Columns[I].MemberName = 'Deleted' then
        Item.SubItems.Add(BoolToStr((LObject as IObjectState).State.Deleted, True))
      else
      begin
        LMember:= LObject.Member[Columns[I].MemberName];
        if LMember.ValueTypeKind = vtBoolean then
          Item.SubItems.Add(TranslateBoolStr(LMember.AsString))
        else
          Item.SubItems.Add(LMember.AsString);
      end;
    end;
  end
end;

procedure TListViewObserver.ListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  I: Integer;
  LMasterObject: IObjectType;
  LDetailObserver: IListViewObserver;
  LMemberItemList: IObjectListType;
begin
  if (Item = nil) or (FList = nil) or (FDetails = nil) or (FDetails.Count = 0) then Exit;
  LMasterObject:= IObjectListType(FList).Items[Item.Index];

  for I:= 0 to FDetails.Count -1 do
  begin
    LDetailObserver:= IListViewObserver(FDetails[I]);
    LDetailObserver.GetListView.Clear;
    LMemberItemList:= LMasterObject.Member[LDetailObserver.MasterLinkName] as IObjectListType; 
    if LMemberItemList <> nil then
      IListViewObserver(LDetailObserver).SetObjectList(LMemberItemList);
  end;
end;

procedure TListViewObserver.SetMasterLink(const Master: IListViewObserver; const MasterMember: string);
begin
  FMaster:= Pointer(Master);
  FMasterLinkName:= MasterMember;
  Master.AddDetail(Self);
end;

procedure TListViewObserver.SetObjectList(const ObjectList: IObjectListType);
begin
  if ObjectList <> nil then
  begin
    if FList <> nil then
    begin
      FList:= nil;
      (ObjectList as ISubject).Detach(Self);
    end;
    
    FList:= Pointer(ObjectList);
    (ObjectList as ISubject).Attach(Self);
  end;
end;

procedure TListViewObserver.Update(const Notification: IObjectEvent);

  procedure CheckItemsCount;
  var
    I: Integer;
    LDetailListView: TListView;
  begin
    if not IObjectListType(FList).IsUpdating then
    begin
      FListView.Items.Count:= IObjectListType(FList).Count;

      if FListView.Items.Count = 0 then
      begin
        FListView.Items.Clear;
        FListView.Repaint;

        if (FDetails <> nil) and (FDetails.Count > 0) then
        begin
          for I:= 0 to FDetails.Count -1 do
          begin
            LDetailListView:= IListViewObserver(FDetails[I]).GetListView;

            if LDetailListView <> nil then
            begin
              LDetailListView.Items.Clear;
              LDetailListView.Repaint;
            end;
          end;
        end;
        FListView.Repaint;
      end;
    end;
  end;
begin
//  SendDebug(InterfaceToObject(Notification).ClassType.ClassName);
  if (FListView = nil) or (FList = nil) then Exit;

  if Notification.NotifyType = ntAttached then
  begin
    FListView.OnData:= ListViewData;
    CheckItemsCount;
  end
  else
  if Notification.NotifyType = ntDetached then
    FListView.OnData:= nil
  else
    CheckItemsCount;
end;

function TListViewObserver.GetSelection: IObjectType;
begin
  if FListView.Selected <> nil then
    Result:= IObjectType(FListView.Selected.Data);
end;

{ TListViewColumn }

constructor TListViewColumn.Create(const MemberName, Caption: string;
  const Width: Integer);
begin
  inherited Create;
  FMemberName:= MemberName;
  FCaption:= Caption;
  FWidth:= Width;
end;

function TListViewColumn.GetCaption: string;
begin
  Result:= FCaption;
end;

function TListViewColumn.GetMemberName: string;
begin
  Result:= FMemberName;
end;

function TListViewColumn.GetWidth: Integer;
begin
  Result:= FWidth;
end;

procedure TListViewColumn.SetCaption(const Value: string);
begin
  FCaption:= Value;
end;

procedure TListViewColumn.SetMemberName(const Value: string);
begin
  FMemberName:= Value;
end;

procedure TListViewColumn.SetWidth(const Value: Integer);
begin
  FWidth:= Value;
end;

procedure TListViewColumn.Update(const Notification: IObjectEvent);
begin
  if FColumn <> nil then TListItems(FColumn.Collection).Owner.Repaint;
end;

end.

