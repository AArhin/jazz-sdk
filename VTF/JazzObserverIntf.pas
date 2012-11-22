unit JazzObserverIntf;

interface

uses
  JazzNotifyIntf;

type
  IObserver = interface(IInterface)
    ['{08932179-BB02-4F92-980D-B46FE5D682C0}']
    procedure Update(const Notification: IObjectEvent);
  end;

implementation

end.




