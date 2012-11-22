unit AppPresenter;

interface

uses
  JazzPresenter,
  JazzSessionIntf,
  JazzSession,
  JazzDBXMechanism;

type
  IAppManager = interface(IInterface)
    ['{92E785BD-AFE2-4357-97EE-479BA62CD667}']
  end;

  TManager = class(TInterfacedObject, IAppManager)
  public
    constructor Create; virtual;
  end;


implementation

{ TManager }

constructor TManager.Create;
begin
  inherited Create;
  
end;

end.
