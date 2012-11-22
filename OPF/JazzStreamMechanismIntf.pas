unit JazzStreamMechanismIntf;

interface

uses
  Classes,
  JazzMechanismIntf,
  JazzValueTypeIntf;

type
  IStreamMechanism = Interface(IMechanism)
    ['{50F5A920-007A-4722-A00A-4FD28A7E24DF}']
    function GetDatabaseFolder: string;
    function GetFileExtension: string;
    procedure SetDatabaseFolder(const Value: string);
    procedure SetFileExtension(const Value: string);
    function GetFileName(const ObjectList: IObjectListType): string;
    function GetFilePath(const FileName: string): string; overload;
    function GetFilePath(const ObjectList: IObjectListType): string; overload;

    procedure LoadFromFile(const FileName: string; const ObjectList: IObjectListType);
    procedure SaveToFile(const FileName: string; const ObjectList: IObjectListType);

    procedure LoadFromStream(Stream: TStream; const ObjectList: IObjectListType);
    procedure SaveToStream(Stream: TStream; const ObjectList: IObjectListType);
    function GetLastError: string;
    procedure SetLastError(const Value: string);

    property FileExtension: string read GetFileExtension write SetFileExtension;
    property DatabaseFolder: string read GetDatabaseFolder write SetDatabaseFolder;
    property LastError: string read GetLastError write SetLastError;
  end;

implementation

end.
