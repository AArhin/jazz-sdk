unit JazzInteractorIntf;

interface

uses
  Classes,
  JazzValueTypeIntf,
  JazzModelIntf,
  JazzViewIntf;

type
  IInteractor = interface(IInterface)
    ['{27A36C81-1A40-47BC-B071-EAAB28501829}']
    function GetOnExecute: TNotifyEvent;
    procedure SetOnExecute(const Value: TNotifyEvent);

    function GetModel: IModel;
    function GetView: IView;

    function IsActive: Boolean;
    function IsDone: Boolean;
    procedure SetDone(Value: Boolean);

    procedure Activate;
    procedure Deactivate;
    procedure Execute(Sender: TObject);

    property Model: IModel read GetModel;
    property View: IView read GetView;
    property OnExecute: TNotifyEvent read GetOnExecute write SetOnExecute;   
  end;

  IInteractorList = interface(IInterface)
    ['{27A36C81-1A40-47BC-B071-EAAB28501829}']
    function GetModel: IModel;
    function GetView: IView;
    function GetItems(const Index: Integer): IInteractor;

    function Add(const InteractorClass: TClass): IInteractor; overload;
    function Add(const Interactor: IInteractor): Integer; overload;
    function Count: Integer;
    function Get(const InteractorClass: TClass): IInteractor; 
    procedure Clear;
    procedure Delete(const Index: Integer);

    property Items[const Index: Integer]: IInteractor read GetItems; default;
    property Model: IModel read GetModel;
    property View: IView read GetView;
  end;

implementation

end.
