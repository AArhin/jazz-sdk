unit JazzBaseTable;
{ DONE -oCesar -cD2009 : Convert to Delphi 2009 }

{$I ..\Lib\Jazz.inc}

interface

uses
  Classes,
  DB,
  DBConsts,
  Forms,
  SysUtils,
  Variants,
  Windows;

type
  PRecordInfo = ^TRecordInfo;
  TRecordInfo = record
    RecordID: Integer;
    Bookmark: Pointer;
    BookMarkFlag: TBookmarkFlag;
  end;

  {$IFDEF VER220}
       erro
  {$ENDIF}

//{$IFDEF DELPHIXE}
    TRecordBuffer = PByte;
//{$ELSE}
//  {$IFNDEF DELPHI12}
//    TRecordBuffer = PAnsiChar;
//  {$ENDIF}
//{$ENDIF}

  TBaseTableMasterDataLink = class(TMasterDataLink)
  protected
    procedure ActiveChanged; override;
  end;

  {$IFDEF D10_OR_HIGHER}
  TBaseTable = class(TWideDataSet)
  {$ELSE}
  TBaseTable = class(TDataSet)
  {$ENDIF}
  private
    FBufferMap: TStringList;
    FCurrent: Integer;
    FIsOpen: Boolean;
    FStartCalculated: Integer;
    procedure FillBufferMap;
    function RecordFilter: Boolean;
    function GetMasterSource: TDataSource;
    procedure SetMasterSource(const Value: TDataSource);
  protected
    FMasterDataLink: TBaseTableMasterDataLink;
    // simplified methods to override
    function DoOpen: Boolean; virtual;
    procedure DoCancelRecord; virtual;
    procedure DoClose; virtual;
    procedure DoCreateFieldDefs; virtual;
    procedure DoDeleteRecord; virtual;
    procedure DoInsertRecord; virtual;
    procedure DoPostRecord; virtual;
    procedure DoSetMasterField(Field: TField); virtual; abstract;
    function GetFieldValue(Field: TField): Variant; virtual; abstract;
    procedure SetFieldValue(Field: TField; Value: Variant); virtual; abstract;
    procedure GetBlobField(Field: TBlobField; Stream: TStream); virtual; abstract;
    procedure SetBlobField(Field: TBlobField; Stream: TStream); virtual; abstract;
    //BookMark functions
    function GetBookMarkSize: Integer; virtual;
    procedure AllocateBookMark(RecordID: Integer; Bookmark: Pointer); virtual;
    //Navigation methods
    procedure SetFiltered(Value: Boolean); override;
    // DataLink for Master/Detail
    procedure MasterChanged(Sender: TObject); virtual;
    procedure MasterDisabled(Sender: TObject); virtual;
    procedure SetDataSetField(const Value: TDataSetField); override;
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    procedure SetMasterAttribute(const Value: string);
    function GetMasterAttribute: string;
    //Internal IsOpen property
    property IsOpen: Boolean read FIsOpen;
    //Internal Current property
    property Current: Integer read FCurrent;
  protected
    //Internal functions that can be overriden if needed
    procedure AllocateBlobPointers(Buffer: TRecordBuffer);
    procedure FreeRecordPointers(Buffer: TRecordBuffer);
    function GetDataSize: Integer; 
    function GetFieldOffSet(Field: TField): Integer; 
    procedure SetRecordId(ABuffer: TRecordBuffer; ABookmarkFlag: TBookmarkFlag);
    procedure BufferToRecord(Buffer: TRecordBuffer);
    procedure RecordToBuffer(Buffer: TRecordBuffer);
  protected
    // Dataset abstract methods (required)
    function AllocRecordBuffer: TRecordBuffer; override;
    function GetActiveRecordBuffer: TRecordBuffer;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetCanModify: Boolean; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetRecordSize: Word; override;
    function IsCursorOpen: Boolean; override;
    procedure ClearCalcFields(Buffer: TRecordBuffer); override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalCancel; override;
    procedure InternalClose; override;
    procedure InternalDelete; override;
    procedure InternalEdit; override;
    procedure InternalFirst; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalInsert; override;
    procedure InternalLast; override;
    procedure InternalOpen; override;
    procedure InternalPost; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;
    function SupportedFieldType(AType: TFieldType): Boolean; virtual;
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;

    property MasterSource: TDataSource read GetMasterSource write SetMasterSource;
    property MasterAttribute: string read GetMasterAttribute write SetMasterAttribute;
  end;

  TObjectTableBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TBaseTable;
    FMode: TBlobStreamMode;
    FModified: Boolean;
    FOpened: Boolean;
    procedure LoadBlobData;
    procedure SaveBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Truncate;
  end;

implementation

uses
  SqlTimSt,
  JazzVarUtils,
  JazzObjectTableConsts;

{ TObjectTableBlobStream }

constructor TObjectTableBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  inherited Create;
  FField:= Field;
  FMode:= Mode;
  FDataSet:= FField.DataSet as TBaseTable;
  if Mode = bmWrite then Truncate else LoadBlobData;
end;

destructor TObjectTableBlobStream.Destroy;
begin
  if FModified then SaveBlobData;
  inherited Destroy;
end;

function TObjectTableBlobStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result:= inherited Read(Buffer, Count);
  FOpened:= True;
end;

function TObjectTableBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result:= inherited Write(Buffer, Count);
  FModified:= True;
end;

procedure TObjectTableBlobStream.LoadBlobData;
var
  LStream: TMemoryStream;
  LOffSet: Integer;
  LRecBuffer: TRecordBuffer;
begin
  Self.Size:= 0;
  LRecBuffer:= FDataSet.GetActiveRecordBuffer;
  if LRecBuffer <> nil then
    begin
      LOffSet:= FDataSet.GetFieldOffSet(FField);
      Move((LRecBuffer + LOffSet)^, Pointer(LStream), SizeOf(Pointer));
      Self.CopyFrom(LStream, 0);
    end;
  Position:= 0;
end;

procedure TObjectTableBlobStream.SaveBlobData;
var
  LStream: TMemoryStream;
  LOffSet: Integer;
  LRecBuffer: TRecordBuffer;
begin
  LRecBuffer:= FDataSet.GetActiveRecordBuffer;
  if LRecBuffer <> nil then
    begin
      LOffSet:= FDataSet.GetFieldOffSet(FField);
      Move((LRecBuffer + LOffSet)^, Pointer(LStream), SizeOf(Pointer));
      LStream.Size:= 0;
      LStream.CopyFrom(Self, 0);
      LStream.Position:= 0;
    end;
  FModified:= False;
end;

procedure TObjectTableBlobStream.Truncate;
begin
  Clear;
  FModified:= True;
end;

{ TBaseTable }

procedure TBaseTable.FillBufferMap;
var
  LIndex: Integer;
  LField: TField;
begin
  FBufferMap.Clear;
  for LIndex:= 0 to FieldCount - 1 do
  begin
    LField:= Fields[LIndex];
    FBufferMap.Add(LField.FieldName);
  end;
end;

procedure TBaseTable.InternalOpen;
begin
  if DoOpen then
  begin
    BookmarkSize:= GetBookMarkSize; //Bookmarks not supported
    InternalInitFieldDefs;
    if DefaultFields then CreateFields;
    BindFields(True);
    FisOpen:= True;
    FillBufferMap;
  end;
end;

function TBaseTable.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, GetRecordSize);
  FillChar(Result^, GetRecordSize, 0);
  AllocateBlobPointers(Result);
end;

procedure TBaseTable.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  FreeRecordPointers(Buffer);
  FreeMem(Buffer, GetRecordSize);
end;

procedure TBaseTable.FreeRecordPointers(Buffer: TRecordBuffer);
  procedure FreeBlobPointers(Buffer: TRecordBuffer);
  var
    LIndex: Integer;
    LOffSet: Integer;
    LFreeObject: TObject;
  begin
    for LIndex:= 0 to FieldCount - 1 do
      if Fields[LIndex].DataType in [ftMemo, ftGraphic] then
        begin
          LOffSet:= GetFieldOffSet(Fields[LIndex]);
          Move((Buffer + LOffSet)^, Pointer(LFreeObject), SizeOf(Pointer));
          if LFreeObject <> nil then FreeAndNil(LFreeObject);
          Move(Pointer(LFreeObject), (Buffer + LOffSet)^, SizeOf(Pointer));
        end;
  end;

begin
  FreeBlobPointers(Buffer);
  if PRecordInfo(Buffer + GetDataSize)^.BookMark <> nil then
    begin
      FreeMem(PRecordInfo(Buffer + GetDataSize)^.BookMark);
      PRecordInfo(Buffer + GetDataSize)^.BookMark:= nil;
    end;
end;

procedure TBaseTable.AllocateBlobPointers(Buffer: TRecordBuffer);
var
  LIndex: Integer;
  LOffSet: Integer;
  LStream: TMemoryStream;
  LField: TField;
begin
  for LIndex:= 0 to FieldCount - 1 do
  begin
    LField:= Fields[LIndex];
    if LField.DataType in [ftMemo, ftGraphic] then
    begin
      LOffSet:= GetFieldOffSet(LField);
      LStream:= TMemoryStream.Create;
      Move(Pointer(LStream), (Buffer + LOffSet)^, SizeOf(Pointer));
    end;
  end;
end;

procedure TBaseTable.AllocateBookMark(RecordID: Integer; Bookmark: Pointer);
begin

end;

procedure TBaseTable.InternalInitFieldDefs;
begin
  DoCreateFieldDefs;
end;

procedure TBaseTable.ClearCalcFields(Buffer: TRecordBuffer);
begin
  FillChar(Buffer[FStartCalculated], CalcFieldsSize, 0);
end;

function TBaseTable.GetActiveRecordBuffer: TRecordBuffer;
begin
  case State of
    dsBrowse:
      if isEmpty then
        Result:= nil
      else
        Result:= ActiveBuffer;
    dsCalcFields:
      Result:= CalcBuffer;
    dsFilter:
      Result:= TempBuffer;
    dsEdit, dsInsert:                    
      Result:= ActiveBuffer;
  else
    Result:= nil;
  end;
end;

function TBaseTable.GetCanModify: Boolean;
begin
  Result:= False;
end;

function TBaseTable.RecordFilter: Boolean;
var
  LSaveState: TDataSetState;
begin
  Result:= True;
  if Assigned(OnFilterRecord) then
  begin
    LSaveState:= SetTempState(dsFilter);
    try
      SetRecordId(TempBuffer, bfCurrent);
      RecordToBuffer(TempBuffer);
      OnFilterRecord(Self, Result);
    except
      InternalHandleException;
    end;
    RestoreState(LSaveState);
  end;
end;

function TBaseTable.GetRecNo: Integer;
var
  LRecBuf: TRecordBuffer;
begin
  Result:= -1;
  LRecBuf:= ActiveBuffer;
  if GetBookmarkFlag(LRecBuf) = bfCurrent then
    Result:= Integer(PRecordInfo(LRecBuf + GetDataSize).RecordID) + 1;
end;

function TBaseTable.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;

  function Navigate: TGetResult;
  begin
    if (RecordCount < 1) then
      Result:= grEOF
    else
    begin
      Result:= grOK;
      case GetMode of
        gmNext:
          if FCurrent >= RecordCount -1 then
            Result:= grEOF
          else
            Inc(FCurrent);
        gmPrior:
          begin
            if FCurrent <= 0 then
            begin
              Result:= grBOF;
              FCurrent:= -1;
            end
          else
            Dec(FCurrent);
          end;
        gmCurrent:
          if (FCurrent < 0) or (FCurrent >= RecordCount) then
            Result:= grError;
      end;
    end;
  end;

var
  LLocalAccept : boolean;
begin
  LLocalAccept:= True;
  repeat
    Result:= Navigate;
    if (Result = grOk) then
    begin
      if Filtered then LLocalAccept:= RecordFilter;

      if LLocalAccept then
      begin
        SetRecordId(Buffer, bfCurrent);
        RecordToBuffer(Buffer);
        ClearCalcFields(Buffer);
        GetCalcFields(Buffer);
      end;
    end
    else
    if (Result = grError) and DoCheck then
      DatabaseError(SNoRecords);

    // TODO: fix filter when edit
    // workaround to avoid frozen  
    if Filtered and not LLocalAccept and (GetMode = gmCurrent) then Break;
  until LLocalAccept or (Result in [grEOF, grBOF]);
end;

function TBaseTable.GetRecordSize: Word;
begin
  Result:= GetDataSize + SizeOf(TRecordInfo) + CalcFieldsSize;
  FStartCalculated:= GetDataSize + SizeOf(TRecordInfo);
end;

function TBaseTable.GetDataSize: Integer;
var
  LIndex: Integer;
begin
  Result:= 0;
  for LIndex:= 0 to FieldCount - 1 do
    case Fields[LIndex].DataType of
      ftString: Result:= Result + Fields[LIndex].Size + 1; //Leave space for terminating null
      ftInteger: Result:= Result + SizeOf(Integer);
      ftSmallInt: Result:= Result + SizeOf(Smallint);
      ftWord: Result:= Result + SizeOf(word);
      ftBoolean: Result:= Result + SizeOf(WordBool);
      ftFloat,
      ftCurrency: Result:= Result + SizeOf(Double);
      ftLargeInt: Result:= Result + SizeOf(int64);
      ftBCD,
      ftDateTime,
      ftDate,
      ftTime,
      ftTimeStamp: Result:= Result + SizeOf(Double);
      ftMemo,
      ftGraphic,
      ftDataSet: Result:= Result + SizeOf(Pointer);
    end;
end;

procedure TBaseTable.InternalClose;
begin
  BindFields(False);
  if DefaultFields then  DestroyFields;
  DoClose;
  FIsOpen:= False;
end;

procedure TBaseTable.InternalDelete;
begin
  DoDeleteRecord;
end;

procedure TBaseTable.InternalEdit;
begin
  if GetActiveRecordBuffer <> nil then
    InternalSetToRecord(GetActiveRecordBuffer);
end;

procedure TBaseTable.InternalFirst;
begin
  FCurrent:= -1;
end;

procedure TBaseTable.InternalHandleException;
begin
  Application.HandleException(Self);
end;

{This is called by the TDataset to initialize an already existing buffer.
We cannot just fill the buffer with 0s sInce that would overwrite our BLOB pointers.
Therefore we free the blob pointers first, then fill the buffer with zeros, then
reallocate the blob pointers}

procedure TBaseTable.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FreeRecordPointers(Buffer);
  FillChar(Buffer^, GetRecordSize, 0);
  AllocateBlobPointers(Buffer);
end;

procedure TBaseTable.InternalInsert;
begin
  UpdateCursorPos;
  DoInsertRecord;
  SetRecordId(ActiveBuffer, bfInserted);
  RecordToBuffer(ActiveBuffer);
  ClearCalcFields(ActiveBuffer);
  GetCalcFields(ActiveBuffer);
end;

procedure TBaseTable.InternalLast;
begin
  FCurrent:= RecordCount;
end;

procedure TBaseTable.InternalPost;
begin
  if FIsOpen then
  begin
    BufferToRecord(GetActiveRecordBuffer);
    DoPostRecord;
  end;
end;

procedure TBaseTable.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if Append then InternalLast;
  BufferToRecord(Buffer);
end;

procedure TBaseTable.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  if GetBookmarkFlag(Buffer) in [bfCurrent, bfInserted] then
    FCurrent:= PRecordInfo(Buffer + GetDataSize).RecordID;
end;

function TBaseTable.IsCursorOpen: Boolean;
begin
  Result:= FisOpen;
end;

function TBaseTable.GetFieldOffSet(Field: TField): Integer;
var
  I: Integer;
  LCount: Integer;
begin
  Result:= 0;
  LCount:= FBufferMap.IndexOf(Field.FieldName);
  for I:= 0 to LCount -1 do
  begin
    case FieldByName(FBufferMap[I]).DataType of
      ftString: Inc(Result, FieldByName(FBufferMap[I]).Size + 1);
      ftInteger: Inc(Result, SizeOf(Integer));
      ftSmallInt: Inc(Result, SizeOf(SmallInt));
      ftWord: Inc(Result, SizeOf(Word));
      ftFloat, ftCurrency: Inc(Result, SizeOf(Double));
      ftLargeInt: Inc(Result, SizeOf(Int64));
      ftBCD, ftDate, ftTime, ftDateTime, ftTimeStamp: Inc(Result, SizeOf(Double));
      ftBoolean: Inc(Result, SizeOf(WordBool));
      ftGraphic, ftMemo, ftDataset: Inc(Result, SizeOf(Pointer));
    end;
  end;
end;

procedure TBaseTable.BufferToRecord(Buffer: TRecordBuffer);
var
  I: Integer;
  LBoolean: WordBool;
  LDate: TDateTimeRec;
  LDouble: Double;
  LField: TField;
  LInteger: Integer;
  LLongInt: Int64;
  LObject: TObject;
  LOffSet: Integer;
  LSmallInt: smallint;
  LStream: TStream;
  LString: AnsiString;
  LTimeStamp: TTimeStamp;
  LValue: Variant;
  LWord: Word;
begin
  for I:= 0 to FieldCount -1 do
  begin
    LField:= Fields[I];
    LOffSet:= GetFieldOffSet(LField);
    case LField.DataType of
      ftString:
        begin
          if Buffer = nil then
            LString:= EmptyStr
          else
            LString:= PAnsiChar(Buffer + LOffSet);

          if LString = EmptyStr then
            SetFieldValue(LField, Null)
          else
            SetFieldValue(LField, LString);
        end;
      ftInteger:
        begin
          if Buffer = nil then
            LInteger:= 0
          else
            Move((Buffer + LOffSet)^, LInteger, SizeOf(Integer));

          if LInteger = 0 then
            SetFieldValue(LField, Null)
          else
            SetFieldValue(LField, LInteger);
        end;
      ftSmallInt:
        begin
          if Buffer = nil then
            LSmallInt:= 0
          else
            Move((Buffer + LOffSet)^, LSmallInt, SizeOf(SmallInt));

          if LSmallInt = 0 then
            SetFieldValue(LField, Null)
          else
            SetFieldValue(LField, LSmallInt);
        end;
      ftWord:
        begin
          if Buffer = nil then
            LWord:= 0
          else
            Move((Buffer + LOffSet)^, LWord, SizeOf(Word));

          if LWord = 0 then
            SetFieldValue(LField, Null)
          else
            SetFieldValue(LField, LWord);
        end;
      ftBoolean:
        begin
          if Buffer = nil then
            SetFieldValue(LField, Null)
          else
          begin
            Move((Buffer + LOffSet)^, LBoolean, SizeOf(WordBool));
            SetFieldValue(LField, LBoolean);
          end;
        end;
      ftBCD, ftFloat, ftCurrency:
        begin
          if Buffer = nil then
            LDouble:= 0.0
          else
            Move((Buffer + LOffSet)^, LDouble, SizeOf(Double));

          if LDouble = 0.0 then
            SetFieldValue(LField, Null)
          else
            SetFieldValue(LField, LDouble);
        end;
      ftLargeInt:
        begin
          if Buffer = nil then
            LLongInt:= 0
          else
            Move((Buffer + LOffSet)^, LLongInt, SizeOf(LLongInt));

          if LLongInt = 0 then
            SetFieldValue(LField, Null)
          else
            SetFieldValue(Fields[I], LLongInt);
        end;
      ftDate, ftDateTime, ftTime, ftTimeStamp:
        begin
          if Buffer = nil then
            SetFieldValue(LField, Null)
          else
          begin
            case LField.DataType of
              ftDate: Move((Buffer + LOffSet)^, LDate.Date, SizeOf(Double));
              ftTime: Move((Buffer + LOffSet)^, LDate.Time, SizeOf(Double));
            else
              Move((Buffer + LOffSet)^, LDate, SizeOf(Double));
            end;

            LTimeStamp:= MSecsToTimeStamp(LDate.DateTime);

            if (Buffer = nil) or
              ((LTimeStamp.Date = 0) and (LTimeStamp.Time = 0)) then
              LDouble:= 0.0
            else
              LDouble:= TimeStampToDateTime(LTimeStamp);

            if LDouble = 0.0 then
              SetFieldValue(LField, Null)
            else
              SetFieldValue(LField, LDouble);
          end;
        end;
      ftGraphic, ftMemo:
        begin
          Move((Buffer + LOffSet)^, Pointer(LStream), SizeOf(Pointer));
          if LStream.Size = 0 then
            SetFieldValue(LField, Null)
          else
          begin
            LStream.Position:= 0;
            SetBlobField(LField as TBlobField, LStream);
          end;
        end;
      ftDataSet:
        begin
          Move((Buffer + LOffSet)^, LObject, SizeOf(TObject));
          TVarData(LValue).VType:= varByRef;
          TVarData(LValue).VPointer:= LObject;
          SetFieldValue(LField, LValue);
        end;
    end;
  end;
end;

procedure TBaseTable.RecordToBuffer(Buffer: TRecordBuffer);
var
  I: Integer;
  LBoolean: WordBool;
  LDate: TDateTimeRec;
  LDateType: TFieldType;
  LDouble: Double;
  LField: TField;
  LInteger: Integer;
  LLongInt: Int64;
  LObject: TObject;
  LOffSet: Integer;
  LSmallInt: Smallint;
  LStream: TStream;
  LString: string;
  LTimeStamp: TTimeStamp;
  LValue: Variant;
  LWord: word;
begin
  for I:= 0 to FieldCount - 1 do
  begin
    LField:= Fields[I]; 
    if not (LField.DataType in [ftMemo, ftGraphic]) then
      LValue:= GetFieldValue(LField);

    LOffSet:= GetFieldOffSet(LField);
    LDateType:= LField.DataType;
    case lDateType of
      ftString:
        begin
          if IsVarNull(LValue) then
            LString:= EmptyStr
          else
            LString:= LValue;

          if Length(LString) > LField.Size then
            System.Delete(LString, LField.Size, Length(LString) - LField.Size);

          StrLCopy(PAnsiChar(Buffer + LOffSet), PAnsiChar(LString), length(LString));
        end;
      ftInteger:
        begin
          if not IsVarNull(LValue) then
          begin
            LInteger:= LValue;
            Move(LInteger, (Buffer + LOffSet)^, SizeOf(LInteger));
          end;
        end;
      ftSmallInt:
        begin
          if IsVarNull(LValue) then
            LSmallInt:= 0
          else
            LSmallInt:= LValue;
          Move(LSmallInt, (Buffer + LOffSet)^, SizeOf(LSmallInt));
        end;
      ftWord:
        begin
          if IsVarNull(LValue) then
            LWord:= 0
          else
            LWord:= LValue;
          Move(LWord, (Buffer + LOffSet)^, SizeOf(LWord));
        end;
      ftLargeInt:
        begin
          if IsVarNull(LValue) then
            LLongInt:= 0
          else
            LLongInt:= LValue;
          Move(LLongInt, (Buffer + LOffSet)^, SizeOf(LLongInt));
        end;
      ftBoolean:
        begin
          if IsVarNull(LValue) then
            LBoolean:= False
          else
            LBoolean:= LValue;
          Move(LBoolean, (Buffer + LOffSet)^, SizeOf(LBoolean));
        end;
      ftBCD,
      ftFloat,
      ftCurrency:
        begin
          if IsVarNull(LValue) then
            LDouble:= 0
          else
            LDouble:= LValue;
          Move(LDouble, (Buffer + LOffSet)^, SizeOf(LDouble));
        end;
      ftDate, ftTime,
      ftDateTime, ftTimeStamp:
        begin
          if IsVarNull(LValue) then
          begin
            LDouble:= 0.0;
            Move(LDouble, (Buffer + LOffSet)^, SizeOf(TDateTimeRec));
          end
          else
          begin
            LDouble:= LValue;
            LTimeStamp:= DateTimeToTimeStamp(LDouble);
            LDate.DateTime:= TimeStampToMSecs(LTimeStamp);
            case lDateType of
              ftDate: Move(LDate.Date, (Buffer + LOffSet)^, SizeOf(TDateTimeRec));
              ftTime: Move(LDate.Time, (Buffer + LOffSet)^, SizeOf(TDateTimeRec));
            else
              Move(LDate, (Buffer + LOffSet)^, SizeOf(TDateTimeRec));
            end;

          end;
        end;
      ftMemo, ftGraphic:
        begin
          Move((Buffer + LOffSet)^, Pointer(LStream), SizeOf(Pointer));
          LStream.Size:= 0;
          LStream.Position:= 0;
          GetBlobField(LField as TBlobField, LStream);
        end;
      ftDataSet:
        begin
          LObject:= TVarData(LValue).VPointer;
          Move(LObject, (Buffer + LOffSet)^, SizeOf(TObject));
        end;
    end;
  end;
end;

procedure TBaseTable.DoDeleteRecord;
begin

end;

function TBaseTable.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  LRecBuffer: TRecordBuffer;
  LSource: TRecordBuffer;
  LTarget: TRecordBuffer;
  LOffSet: Integer;

  LDate: TDateTimeRec;
  LLongInt: Largeint;
  LDouble: Double;
begin
  Result:= False;
  if not FIsOpen then Exit;
  LRecBuffer:= GetActiveRecordBuffer;
  if LRecBuffer = nil then Exit;
  if Buffer = nil then
  begin
    Result:= True;
    Exit;
  end;
  
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
  begin
    Inc(LRecBuffer, FStartCalculated + Field.OffSet);

    if (AnsiChar(LRecBuffer[0]) = #0) or (Buffer = nil) then
      Exit
    else
      CopyMemory(Buffer, @LRecBuffer[1], Field.DataSize);
      
    Result:= True;
  end
  else
  if State in [dsFilter, dsBrowse, dsEdit, dsInsert, dsCalcFields] then
  begin
    LOffSet:= GetFieldOffSet(Field);
    LTarget:= TRecordBuffer(Buffer);
    LSource:= (LRecBuffer + LOffSet);
    case Field.DataType of
      ftDataSet:
        Move(LSource^, LTarget^, SizeOf(TObject));

      ftDate, ftTime, ftTimeStamp, ftDateTime:
        begin
          Move(LSource^, LDate, Field.DataSize);
          Result:= (LDate.Date <> 0) and (LDate.Time <> 0);
          if Result then
          begin
             try
               case Field.DataType  of
                 ftDate     : Move(LDate.Date, LTarget^, Field.DataSize);
                 ftTime     : Move(LDate.Time, LTarget^, Field.DataSize);
                 ftDateTime ,
                 ftTimeStamp: Move(LDate, LTarget^, Field.DataSize);
               end;
             except
               Result:= False;
             end;
          end;
        end;
      ftWord, ftSmallInt, ftInteger, ftLargeInt:
        begin
          LLongInt:= 0;
          Move(LSource^, LLongInt, Field.DataSize);
          Result:= LLongInt <> 0;
          if Result then Move(LLongInt, LTarget^, Field.DataSize);
        end;
      ftBCD, ftFloat, ftCurrency:
        begin
          Move(LSource^, LDouble, Field.DataSize);
          Result:= LDouble <> 0.0;
          if Result then Move(LDouble, LTarget^, Field.DataSize);
        end
    else
      Move(LSource^, LTarget^, Field.DataSize);
      Result:= True;
    end;
  end;
end;

procedure TBaseTable.SetFieldData(Field: TField; Buffer: Pointer);
var
  LOffSet: Integer;
  LSource: TRecordBuffer;
  LTarget: TRecordBuffer;
  LRecBuffer: TRecordBuffer;
begin
  if not Active then Exit;
  LRecBuffer:= GetActiveRecordBuffer;
  if (LRecBuffer = nil) or (Buffer = nil) then Exit;
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
  begin
    Inc(LRecBuffer, FStartCalculated + Field.OffSet);
    Boolean(LRecBuffer[0]):= (Buffer <> nil);
    if Boolean(LRecBuffer[0]) then
      CopyMemory(@LRecBuffer[1], Buffer, Field.DataSize);
  end
  else
  begin
    LOffSet:= GetFieldOffSet(Field);
    LSource:= TRecordBuffer(Buffer);
    LTarget:= (LRecBuffer + LOffSet);
    case Field.DataType of
      ftDataset: Move(LSource^, LTarget^, SizeOf(TObject));
    else
      Move(LSource^, LTarget^, Field.Datasize);
    end;
  end;
  
  if not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
end;

function TBaseTable.GetBookMarkSize: Integer;
begin
  Result:= SizeOf(Integer);
end;

procedure TBaseTable.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  if BookMarkSize > 0 then
    AllocateBookMark(PRecordInfo(Buffer + GetDataSize).RecordID, Data);
end;

function TBaseTable.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result:= PRecordInfo(Buffer + GetDataSize).BookMarkFlag;
end;

procedure TBaseTable.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  if PRecordInfo(Buffer + GetDataSize)^.BookMark = nil then
    GetMem(PRecordInfo(Buffer + GetDataSize)^.BookMark, GetBookMarkSize);
  Move(PRecordInfo(Buffer + GetDataSize).BookMark^, Data, GetBookMarkSize);
end;

procedure TBaseTable.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecordInfo(Buffer + GetDataSize).BookMarkFlag:= Value;
end;

procedure TBaseTable.InternalGotoBookmark(Bookmark: Pointer);
begin
  FCurrent:= PInteger(Bookmark)^;
end;

constructor TBaseTable.Create(AOwner: TComponent);
begin
  inherited;
  ObjectView:= True;
  FBufferMap:= TStringList.Create;
  FMasterDataLink:= TBaseTableMasterDataLink.Create(Self);
  FMasterDataLink.OnMasterChange:= MasterChanged;
  FMasterDataLink.OnMasterDisable:= MasterDisabled;
end;

function TBaseTable.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result:= TObjectTableBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TBaseTable.SetFiltered(Value: Boolean);
begin
  inherited;
  First;
end;

procedure TBaseTable.InternalCancel;
begin
  DoCancelRecord;
end;

procedure TBaseTable.DoInsertRecord;
begin
end;

procedure TBaseTable.DoCancelRecord;
begin
end;

procedure TBaseTable.DoClose;
begin
  FCurrent:= -1;
end;

function TBaseTable.GetMasterSource: TDataSource;
begin
  Result:= FMasterDataLink.DataSource;
end;

procedure TBaseTable.SetMasterSource(
  const Value: TDataSource);
begin
  if IsLinkedTo(Value) then DatabaseError(SCircularDataLink, Self);
  FMasterDataLink.DataSource:= Value;
end;

procedure TBaseTable.MasterChanged(Sender: TObject);
var
  LField: TField;
begin
  LField:= MasterSource.DataSet.FindField(FMasterDataLink.FieldNames);
  if (LField <> nil) then DoSetMasterField(LField);
  if Active then Resync([]);
end;

procedure TBaseTable.MasterDisabled(Sender: TObject);
begin
  if Active then Resync([]);
end;

procedure TBaseTable.SetRecNo(Value: Integer);
begin
  if (Value < 1) then
    Value:= 1
  else
    if (Value >= RecordCount) then Value:= RecordCount;

  if (RecNo <> Value) then
  begin
    DoBeforeScroll;
    FCurrent:= Value -1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TBaseTable.SetRecordId(ABuffer: TRecordBuffer; ABookmarkFlag: TBookmarkFlag);
begin
  with PRecordInfo(ABuffer + GetDataSize)^ do
  begin
    BookmarkFlag:= ABookmarkFlag;
    RecordID:= FCurrent;
    if GetBookMarkSize > 0 then
    begin
      if BookMark = nil then GetMem(BookMark, GetBookMarkSize);
      AllocateBookMark(RecordID, BookMark);
    end
    else
      BookMark:= nil;
  end;
end;

function TBaseTable.DoOpen: Boolean;
var
  LField: TField;
begin
  Result:= True;
  if (MasterSource <> nil) and (MasterSource.DataSet <> nil) and (FMasterDataLink.FieldNames <> '') then
  begin
    if (MasterSource.DataSet.Active) then
    begin
      LField:= MasterSource.DataSet.FindField(FMasterDataLink.FieldNames);
      if (LField <> nil) then
        DoSetMasterField(LField)
      else
        DatabaseError(SFieldNotFound);
    end
    else
      DatabaseError(SDataSetClosed, MasterSource.DataSet);
  end;
  FCurrent:= -1;
end;

procedure TBaseTable.SetDataSetField(const Value: TDataSetField);
begin
  if Assigned(Value) then DoSetMasterField(Value);
  inherited SetDataSetField(Value);
end;

procedure TBaseTable.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  if Event = deParentScroll then
  begin
    if Assigned(DatasetField) then
    begin
      DoSetMasterField(DatasetField);
      Resync([]);
    end;
  end;

  inherited DataEvent(Event, Info);
end;

destructor TBaseTable.Destroy;
begin
  if Active then Close;
  FBufferMap.Free;
  FMasterDataLink.Free;
  inherited;
end;

procedure TBaseTable.DoCreateFieldDefs;
  procedure InitFieldDefsFromFields(Fields: TFields; FieldDefs: TFieldDefs);
  var
    I: integer;
    LField: TField;
    LFieldDef: TFieldDef;
  begin
    for I:= 0 to Fields.Count - 1 do
    begin
      LField:= Fields[I];
      with LField do
        if FieldDefs.IndexOf(FieldName) = -1 then
        begin
          LFieldDef:= FieldDefs.AddFieldDef;
          LFieldDef.Name:= FieldName;
          LFieldDef.DataType:= DataType;
          LFieldDef.Size:= Size;
          if Required then LFieldDef.Attributes:= [faRequired];
          if ReadOnly then
            LFieldDef.Attributes:= LFieldDef.Attributes + [db.faReadonly];
          if (DataType = ftBCD) and (LField is TBCDField) then
            LFieldDef.Precision:= TBCDField(LField).Precision;
          if LField is TObjectField then
            InitFieldDefsFromFields(TObjectField(LField).Fields, LFieldDef.ChildDefs);
        end;
    end;
  end;

begin
  FieldDefs.Clear;
  InitFieldDefsFromFields(Fields, FieldDefs);
end;

function TBaseTable.GetMasterAttribute: string;
begin
  Result:= FMasterDataLink.FieldNames;
end;

procedure TBaseTable.SetMasterAttribute(const Value: string);
begin
  FMasterDataLink.FieldNames:= Value;
end;

procedure TBaseTable.DoPostRecord;
begin
end;

function TBaseTable.SupportedFieldType(AType: TFieldType): Boolean;
begin
  Result:= True;
end;

{ TSnapMasterDataLink }

procedure TBaseTableMasterDataLink.ActiveChanged;
begin
  if (DataSet = nil) or (csDestroying in DataSet.ComponentState) then  Exit;
  if not DataSet.Active then Exit;
  if Fields.Count = 0 then Fields.Add(TField.Create(DataSet));

  if Active then
  begin
    if Assigned(OnMasterChange) then OnMasterChange(Self);
  end
  else
    if Assigned(OnMasterDisable) then OnMasterDisable(Self);
end;

end.
