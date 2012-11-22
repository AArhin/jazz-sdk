unit JazzNotifyIntf;

interface

type
  TNotifyType = (
    ntMessage,

    ntAttached,
    ntDetached,

    ntStateChanged,
      ntDeleted,
      ntDeleting,
      ntCancelDeleting,

      ntModified,
      ntLoaded,
      ntPersisted,

    ntSelected,
    ntUnselected,
    ntClearSelection,
    
    ntBeginUpdate,
    ntEndUpdate,

    ntAdd,
    ntInsert,
    ntRemove,
    ntClear
  );

  IObjectEvent = interface(IInterface)
    ['{1DC0CD68-234E-4DB0-A17D-8DD788BE4BE6}']
    function GetNotifyType: TNotifyType;
    function GetParam: IInterface; // ony one or first params
    function GetParams(const Index: Integer): IInterface;
    function GetSender: IInterface;

    procedure AddParam(const Param: IInterface);
    procedure ClearParams;
    procedure RemoveParam(const Param: IInterface);

    procedure StartNotification;
    procedure EndNotification;

    property Param: IInterface read GetParam;
    property Params[const Index: Integer]: IInterface read GetParams; default;
    property NotifyType: TNotifyType read GetNotifyType; 
    property Sender: IInterface read GetSender;
  end;

  // observer and subject
  IAttachedEvent = interface(IObjectEvent)
    ['{CE00B4B8-1A2E-4A20-8725-D8BAA62900DC}']
  end;
  IDetachedEvent = interface(IObjectEvent)
    ['{8F3225DC-E494-459D-A628-81FB1E675802}']
  end;

  // ObjectList/Object/Member
  IClearEvent = interface(IObjectEvent)
    ['{5EBF4CE2-BC5D-40E0-BC08-1734874DD87E}']
  end;

  IObjectListEvent = interface(IObjectEvent)
    ['{E1F0D189-3A41-4E48-ACBA-53CB6AD53954}']
  end;

  //ObjectList
  IAddEvent = interface(IObjectListEvent)
    ['{86FB9AE0-516E-49B5-9E58-75A6C7C6C740}']
  end;

  IInsertEvent = interface(IObjectListEvent)
    ['{A472B11E-E141-498B-95D8-FCD90B8725FC}']
  end;

  IRemoveEvent = interface(IObjectListEvent)
    ['{71D18D2F-20E1-4E72-9119-3DFAE8FAC581}']
  end;

  IBeginUpdateEvent = interface(IObjectListEvent)
    ['{52718153-BB98-4313-83D5-85CB6AFD29C5}']
  end;

  IEndUpdateEvent = interface(IObjectListEvent)
    ['{EEF96A03-58EE-4162-9E3E-45F0F4FABFA5}']
  end;

  INotifyManager = interface(IInterface)
    ['{91544CEF-181F-4080-ACBA-298CA303A5ED}']
    procedure AddNotifier(const Item: IObjectEvent);
    procedure RemoveNotifier(const Item: IObjectEvent);
    procedure Clear;
  end;

implementation

end.

