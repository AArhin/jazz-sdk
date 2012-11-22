unit JazzStreamMechanism;

interface

uses
  Classes,
  JazzMechanism,
  JazzStreamMechanismIntf,
  JazzValueTypeIntf,
  JazzMappingIntf,
  JazzCriteriaIntf;

type
  TStreamMechanism = class(TMechanism, IStreamMechanism)
  private
    FFileExtension: string;
    FDatabaseFolder: string;
    FLastError: string;
    function GetFileName(const ObjectList: IObjectListType): string;
    function GetFilePath(const FileName: string): string; overload;
    function GetFilePath(const ObjectList: IObjectListType): string; overload;
    procedure RaiseTransactionNotSuported;
  protected
    function GetDatabaseFolder: string;
    function GetFileExtension: string;
    procedure SetDatabaseFolder(const Value: string);
    procedure SetFileExtension(const Value: string);

    function LoadMember(const Member: IMemberType; const AutoLoad: boolean = False): boolean; override;
    function LoadObject(const AObject: IObjectType; const Meta: IObjectMeta; const Members: TStrings = nil): boolean; override;
    function LoadObjectList(const AObjectList: IObjectListType; const Criteria: ICriteria = nil): boolean; override;

    procedure SaveObject(const AObject: IObjectType; const Meta: IObjectMeta = nil); override;
    procedure SaveObjectList(const  AObjectList: IObjectListType); override;

    procedure LoadFromStream(Stream: TStream; const ObjectList: IObjectListType); virtual;
    procedure SaveToStream(Stream: TStream; const ObjectList: IObjectListType); virtual;

    procedure LoadFromFile(const FileName: string; const ObjectList: IObjectListType); virtual;
    procedure SaveToFile(const FileName: string; const ObjectList: IObjectListType); virtual;

    function InTransaction: boolean; override;
    procedure StartTransaction; override;
    procedure CommitTransaction(const ForceCommit: boolean = False); override;
    procedure RollbackTransaction; override;
    function GetLastError: string;
    procedure SetLastError(const Value: string);

    property FileExtension: string read GetFileExtension write SetFileExtension;
    property DatabaseFolder: string read GetDatabaseFolder write SetDatabaseFolder;
    property LastError: string read GetLastError write SetLastError;
  public
    constructor Create(const Session: IInterface); override;
  end;

var
  FileNameSufix: string = 'List';

implementation

uses
  SysUtils,
  Windows,
  JazzPersisterConsts,
  JazzClasses;

constructor TStreamMechanism.Create(const Session: IInterface);
begin
  inherited Create(Session);
  FDatabaseFolder:= SDatabaseFolder;
  FileExtension:= SFileExtension;
end;

procedure TStreamMechanism.CommitTransaction(const ForceCommit: boolean = False);
begin
  RaiseTransactionNotSuported;
end;

function TStreamMechanism.GetDatabaseFolder: string;
begin
  Result:= FDatabaseFolder;
end;

function TStreamMechanism.GetFileExtension: string;
begin
  Result:= FFileExtension;
end;

function TStreamMechanism.GetFileName(const ObjectList: IObjectListType): string;
var
  S: string;
begin
  S:= ObjectList.ItemClass.ClassName;
  Result:= Copy(S, 2, Length(S) -1) + FileNameSufix;
end;

function TStreamMechanism.GetFilePath(const FileName: string): string;
begin
  Result:= IncludeTrailingPathDelimiter(DatabaseFolder) + FileName + 
   FileExtension;
end;

function TStreamMechanism.InTransaction: boolean;
begin
  Result:= False;
  RaiseTransactionNotSuported;
end;

procedure TStreamMechanism.LoadFromFile(const FileName: string;
  const ObjectList: IObjectListType);
var
  LFile: string;
  LStream: TFileStream;
begin
  LFile:= GetFilePath(FileName);
  LastError:= '';
  if not FileExists(LFile) then
  begin
    LastError:= Format(SFileNotFound, [LFile]);
    Exit;
  end;

  LStream:= TFileStream.Create(LFile, fmOpenRead);
  try
    LoadFromStream(LStream, ObjectList);
  finally
    LStream.Free;
  end;
end;

procedure TStreamMechanism.LoadFromStream(Stream: TStream;
  const ObjectList: IObjectListType);
var
  LReader: TReader;
  LObjectList: IStreamable;
begin
  LastError:= '';
  if Supports(ObjectList, IStreamable, LObjectList) then
  begin
    LReader:= TReader.Create(Stream, $ff);
    try
      LObjectList.LoadFromStream(LReader);
    finally
      LReader.Free;
    end;
  end;
end;

function TStreamMechanism.LoadMember(const Member: IMemberType;
  const AutoLoad: boolean): boolean;
begin
  Result:= False;
  LastError:= SOnlyListCanBeLoaded;
end;

function TStreamMechanism.LoadObject(const AObject: IObjectType;
  const Meta: IObjectMeta; const Members: TStrings = nil): boolean; 
begin
  Result:= False;
  LastError:= SOnlyListCanBeLoaded;
end;

function TStreamMechanism.LoadObjectList(const AObjectList: IObjectListType;
  const Criteria: ICriteria = nil): boolean;
begin
  LoadFromFile(GetFileName(AObjectList), AObjectList);
  Result:= True;
end;

procedure TStreamMechanism.RaiseTransactionNotSuported;
begin
  LastError:= Format(STransactionNotSupported, [ClassName]);
end;

procedure TStreamMechanism.RollbackTransaction;
begin
  RaiseTransactionNotSuported;
end;

procedure TStreamMechanism.SaveObject(const AObject: IObjectType;
  const Meta: IObjectMeta);
begin
//  inherited SaveObject(AObject, Meta);
end;

procedure TStreamMechanism.SaveObjectList(const  AObjectList: IObjectListType);
begin
  SaveToFile(GetFileName(AObjectList), AObjectList);
end;

procedure TStreamMechanism.SaveToFile(const FileName: string;
  const ObjectList: IObjectListType);
var
  LStream: TFileStream;
begin
  LastError:= '';
  LStream:= TFileStream.Create(GetFilePath(FileName), fmCreate);
  try
    SaveToStream(LStream, ObjectList);
  finally
    LStream.Free;
  end;
end;

procedure TStreamMechanism.SaveToStream(Stream: TStream; const ObjectList: IObjectListType);
var
  LWriter: TWriter;
  LObjectList: IStreamable;
begin
  LastError:= '';
  if Supports(ObjectList, IStreamable, LObjectList) then
  begin
    LWriter:= TWriter.Create(Stream, $ff);
    try
      LObjectList.SaveToStream(LWriter);
    finally
      LWriter.Free;
    end;
  end;
end;

procedure TStreamMechanism.SetDatabaseFolder(const Value: string);
begin
  FDatabaseFolder:= Value;
end;

procedure TStreamMechanism.SetFileExtension(const Value: string);
begin
  if Copy(Value, 1, 1) = '.' then
    FFileExtension:= Value
  else
    FFileExtension:= '.' + Value;
end;

procedure TStreamMechanism.StartTransaction;
begin
  RaiseTransactionNotSuported;
end;

function TStreamMechanism.GetFilePath(const ObjectList: IObjectListType): string;
begin
  Result:= GetFilePath(GetFileName(ObjectList as IObjectListType));
end;

function TStreamMechanism.GetLastError: string;
begin
  Result := FLastError;
end;

procedure TStreamMechanism.SetLastError(const Value: string);
begin
  FLastError := Value;
end;

end.
