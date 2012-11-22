unit JazzMvpNotify;

interface

uses
  JazzNotify,
  JazzNotifyIntf;

type
  IAddSelection = interface(IAddEvent)
    ['{47881B42-984D-4C10-8F50-DB8DA75918E9}']
  end;

  IRemoveSelection = interface(IRemoveEvent)
    ['{AB87AC6A-2E4E-4737-B0C0-C0C77F66580F}']
  end;

  IClearSelection = interface(IClearEvent)
    ['{B0373747-4E22-4859-8422-0AB99B80320C}']
  end;
  
  TAddSelection = class(TObjectEvent, IAddSelection)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TRemoveSelection = class(TObjectEvent, IRemoveSelection)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

  TClearSelection = class(TObjectEvent, IClearSelection)
  protected
    function GetNotifyType: TNotifyType; override;
  end;

implementation

{ TAddSelection }

function TAddSelection.GetNotifyType: TNotifyType;
begin
  Result:= ntSelected;
end;

{ TRemoveSelection }

function TRemoveSelection.GetNotifyType: TNotifyType;
begin
  Result:= ntUnselected;
end;

{ TClearSelection }

function TClearSelection.GetNotifyType: TNotifyType;
begin
  Result:= ntClearSelection;
end;

end.
