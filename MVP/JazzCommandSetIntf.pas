unit JazzCommandSetIntf;

interface

uses
  JazzNotifyIntf;

type
  ICommand = interface(IInterface)
    ['{B993720B-B421-41F9-94BC-0C7A4C69116B}']
    function GetEnabled: boolean;
    function GetName: string;
    procedure SetEnabled(const Value: boolean);

    procedure Execute(const Target: IInterface = nil); 

    property Enabled: boolean read GetEnabled write SetEnabled;
    property Name: string read GetName;
  end;
  
  ICommandSet = interface(IInterface)
    ['{EE2B1585-2239-4751-B41B-32A6EFF1ED5B}']
    function Add(const CommandClass: TClass): ICommand; overload;
    function Add(const Command: ICommand): Integer; overload;
    function GetCount: Integer;
    function GetItem(const Name: string): ICommand;
    function GetItems(const Index: Integer): ICommand;
    procedure Clear;
    procedure DisableAll;
    procedure EnableAll;
    procedure Remove(const Command: ICommand);
    property Count: Integer read GetCount;
    property Item[const Name: string]: ICommand read GetItem;
    property Items[const Index: Integer]: ICommand read GetItems; default;
  end;

implementation

end.

