unit FirstPersisterView;

interface

uses
  JazzDatabaseMechanismIntf,
  JazzNotifyIntf,
  JazzValueTypeIntf,
  JazzView,
  FirstPersisterViewIntf,
  DB;

type
  TObjectListStatusView = class(TObjectListView, IPersonListStatusView)
  private
    FCaption: string;
    function GetCaption: string;
    procedure SetCaption(const Value: string);
  protected
    procedure Update(const Notification: IObjectEvent); override;
    property Caption: string read GetCaption write SetCaption;
  public
    constructor Create(const Control: TObject; const Caption: string); reintroduce; virtual;
  end;

implementation

uses
  JazzNotify,
  JazzSession,
  JazzValueType,
  JazzObserverIntf,
  FirstPersisterPresenter,
  ComCtrls,
  SysUtils;

{ TObjectListStatusView }

constructor TObjectListStatusView.Create(const Control: TObject;
  const Caption: string);
begin
  inherited Create(Control);
  FCaption:= Caption;
end;

function TObjectListStatusView.GetCaption: string;
begin
  Result:= FCaption;
end;

procedure TObjectListStatusView.SetCaption(const Value: string);
begin
  FCaption:= Value;
end;

procedure TObjectListStatusView.Update(const Notification: IObjectEvent);
var
  LObjectList: IObjectListType;
begin
  inherited;
  if Control <> nil then
  begin
    if (Notification.NotifyType = ntDetached) then
      TStatusPanel(Control).Text:= Format('%s %d', [Caption, 0])
    else
    if  Supports(Notification.Sender, IObjectListType, LObjectList) then
      TStatusPanel(Control).Text:= Format('%s %d', [Caption, LObjectList.Count])
  end;
end;

end.
