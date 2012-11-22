unit JazzViewIntf;

interface

uses
  JazzValueTypeIntf,
  JazzObserverIntf,
  JazzModelIntf,
  JazzNotify;

type
  IView = interface(IInterface)
    ['{6E619BE3-AC77-479F-ACF8-DBDA78FD4212}']
    function GetControl: TObject;
    procedure SetControl(const Value: TObject);
    property Control: TObject read GetControl write SetControl;

    // Events
    function GetOnAddSelection: TObjectNotifyEvent;
    function GetOnClearSelection: TObjectNotifyEvent;
    function GetOnRemoveSelection: TObjectNotifyEvent;
    procedure SetOnAddSelection(const Value: TObjectNotifyEvent);
    procedure SetOnClearSelection(const Value: TObjectNotifyEvent);
    procedure SetOnRemoveSelection(const Value: TObjectNotifyEvent);

    procedure DoAddSelection(const AObject: IValueType = nil);
    procedure DoRemoveSelection(const AObject: IValueType = nil);
    procedure DoClearSelection;

    property OnAddSelection: TObjectNotifyEvent read GetOnAddSelection write SetOnAddSelection;
    property OnRemoveSelection: TObjectNotifyEvent read GetOnRemoveSelection write SetOnRemoveSelection;
    property OnClearSelection: TObjectNotifyEvent read GetOnClearSelection write SetOnClearSelection;
  end;

  IValueTypeView = interface(IView)
    ['{EE4F6D93-730B-4363-B318-811363483CC5}']
    function GetObjectValue: IValueType;
    property ObjectValue: IValueType read GetObjectValue;
  end;
                                 
  IMemberView = interface(IValueTypeView)
    ['{96ED0EDF-A481-43A3-AA49-0551BDF5DF48}']
    function GetOnChanged: TObjectNotifyEvent;
    procedure SetOnChanged(const Value: TObjectNotifyEvent);
    property OnChanged: TObjectNotifyEvent read GetOnChanged write SetOnChanged;
  end;

  IBlobTypeView = interface(IMemberView)
    ['{2B5B8331-165E-4CAD-AB1D-36C2B7CBA41A}']
  end;

  IBooleanTypeView = interface(IMemberView)
    ['{F2E1528E-6118-4BB4-8FCB-78F0C0D3AC1B}']
    procedure DoChanged(const Value: boolean);
  end;

  IObjectTypeView = interface(IValueTypeView)
    ['{37535386-13B5-455C-BCF9-9D7DFCA211F5}']
  end;

  IObjectListTypeView = interface(IValueTypeView)
    ['{55557A7C-0750-4C75-8023-941F1F30B2B7}']
    function GetOnAppend: TObjectNotifyEvent;
    function GetOnClear: TObjectNotifyEvent;
    function GetOnDelete: TObjectNotifyEvent;
    function GetOnInsert: TObjectNotifyEvent;
    function GetOnPost: TObjectNotifyEvent;
    procedure SetOnAppend(const Value: TObjectNotifyEvent);
    procedure SetOnClear(const Value: TObjectNotifyEvent);
    procedure SetOnDelete(const Value: TObjectNotifyEvent);
    procedure SetOnInsert(const Value: TObjectNotifyEvent);
    procedure SetOnPost(const Value: TObjectNotifyEvent);

    function DoAppend: IObjectType;
    function DoInsert: IObjectType;
    procedure DoClear;
    procedure DoDelete;

    property OnAppend: TObjectNotifyEvent read GetOnAppend write SetOnAppend;
    property OnInsert: TObjectNotifyEvent read GetOnInsert write SetOnInsert;
    property OnClear: TObjectNotifyEvent read GetOnClear write SetOnClear;
    property OnDelete: TObjectNotifyEvent read GetOnDelete write SetOnDelete;
    property OnPost: TObjectNotifyEvent read GetOnPost write SetOnPost;
  end;

implementation

end.
