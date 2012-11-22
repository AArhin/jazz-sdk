unit JazzSubjectIntf;

interface

uses
  JazzNotifyIntf,
  JazzObserverIntf;

type
  ISubject = interface(IInterface)
    ['{04DB6EEC-4EE1-44F8-B855-A4D13A1BA063}']
    procedure Attach(const Observer: IObserver);
    procedure Detach(const Observer: IObserver);
    procedure DetachAll;
    procedure Notify(const Notification: IObjectEvent);
    function GetNotifying: boolean;
    property Notifying: boolean read GetNotifying;
  end;

implementation

end.


