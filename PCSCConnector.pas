{******************************************************************}
{                                                                  }
{ PC/SC Interface component                                        }
{ Helps you access a cardreader through Microsofts SmartCard API   }
{                                                                  }
{ The Original Code is PCSCConnector.pas                           }
{                                                                  }
{ The Initial Developer of the Original Code is                    }
{ Norbert Huettisch (nobbi(at)nobbi.com)                           }
{                                                                  }
{ Any suggestions and improvements to the code are appreciated     }
{                                                                  }
{ This Code uses a modified   SCardErr.pas (included)              }
{ This Code uses a modified   WinSCard.pas (included)              }
{ This code uses the original WinSmCrd.pas (included)              }
{                                                                  }
{ All originally made by Chris Dickerson (chrisd(at)tsc.com),      }
{ available as 'Interface units for the Microsoft Smart Card API'  }
{ at the Project JEDI Homepage http://www.delphi-jedi.org          }
{                                                                  }
{ Version info:                                                    }
{ 021230 - initial version                                         }
{ 030101 - routed errors from 'init' to the OnError event          }
{                                                                  }
{ Forked version: bits and pieces from various forks               }
{                                                                  }
{******************************************************************}
{                                                                  }
{ The contents of this file are subject to the                     }
{                                                                  }
{       Mozilla Public License Version 1.1 (the "License")         }
{                                                                  }
{ You may not use this file except in compliance with the License. }
{ You may obtain a copy of the License at                          }
{ http://www.mozilla.org/MPL/                                      }
{                                                                  }
{ Software distributed under the License is distributed on an      }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or   }
{ implied. See the License for the specific language governing     }
{ rights and limitations under the License.                        }
{                                                                  }
{******************************************************************}

unit PCSCConnector;

interface

uses
  Windows, Messages, Forms, Classes, SysUtils,
  SCardErr, WinSCard, WinSmCrd, CardUtils, defs;

type
  TErrSource         = (esInit, esConnect, esGetStatus, esTransmit);
  TNeededPIN         = (npPIN1, npPIN2, npPUK1, npPUK2);
  TDelimiters        = set of Char;

  TPCSCErrorEvent    = procedure(Sender: TObject; ErrSource: TErrSource; ErrCode: cardinal) of object;
  TPCSCPinEvent      = procedure(Sender: TObject; NeedPIN: TNeededPIN) of object;

const
  MAXAPDULENGTH      = 260; // CLA + INS + P1..3 + 255Bytes
  NOREADERSELECTED   = -1;
  SCARD_PCI_T0       : SCARD_IO_REQUEST = (dwProtocol:1; dbPciLength:8);
  SCARD_PCI_T1       : SCARD_IO_REQUEST = (dwProtocol:2; dbPciLength:8);
  SCARD_PROTOCOL_T0  = $00000001;
  SCARD_PROTOCOL_T1  = $00000002;
  SCARD_PROTOCOL_RAW = $00010000;
  SCARD_PROTOCOL_UNK = $00000000;
  WM_CARDSTATE     = WM_USER + 42;


type
  TPCSCConnector = class;

  TReaderWatcher = class(TThread)
  private
    FOwner: TPCSCConnector;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TPCSCConnector);
  end;

  TPCSCConnector = class(TComponent)
  private
    function GetAttrATRHistBytes: string;
    function GetATR: ATRRec;
    function GetSelectedReaderName: String;
    function ConnectCardInSelectedReader: boolean;
    procedure ReleaseContext;
    procedure ClearCardAttributes;

  protected
    FContext            : cardinal;
    FCardHandle         : integer;
    FConnected          : boolean;
    FNumReaders         : integer;
    FUseReaderNum       : integer;

    FReaderList         : TStringlist;

    FAPDULogging        : boolean;

    FNotifyHandle       : HWND;
    FAttrProtocol       : integer;
    FAttrICCType        : string;
    FAttrCardATR        : AnsiString;
    FAttrVendorName     : string;
    FAttrVendorSerial   : string;
    FAttrInterfaceStatus: integer;
    FAttrDefaultDataRate: integer;
    FAttrDefaultCLK     : integer;

    FGSMCurrentFile     : string;
    FGSMFileInfo        : string;
    FGSMDirInfo         : string;
    FGSMVoltage30       : boolean;
    FGSMVoltage18       : boolean;

    FActReaderState     : cardinal;
    FLastReaderState    : cardinal;


    FReaderWatcher      : TReaderWatcher;
    FOnReaderWaiting    : TNotifyEvent;
    FOnReaderListChange : TNotifyEvent;
    FOnCardInserted     : TNotifyEvent;
    FOnCardActive       : TNotifyEvent;
    FOnCardRemoved      : TNotifyEvent;
    FOnCardInvalid      : TNotifyEvent;
    FOnError            : TPCSCErrorEvent;
    FOnCHVNeeded        : TPCSCPinEvent;

    procedure SetReaderNum(Value: integer);
    procedure MessageWndProc(var Msg: TMessage);
    function  ConnectSelectedReader: boolean;
    procedure ProcessReaderState(const OldState,NewState: cardinal);
    procedure GetReaderAttributes;
    procedure ClearReaderAttributes;
    function  IsReaderOpen: boolean;
    function  GetReaderState: cardinal;
    procedure CloseAndDisconnect;
    procedure CardInsertedAction;
    procedure CardActiveAction;
    procedure CardRemovedAction;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function   Init: boolean;
    procedure  Close;
    function   Open: boolean;
    function   ConnectCard: boolean;
    procedure  DisconnectCard;
    function   GetResponseFromCard(const apdu: AnsiString): AnsiString; overload;
    function   GetResponseFromCard(const command: AnsiString; var data: AnsiString; var sw: word): boolean; overload;
    function   CardSelect(const aid: AnsiString; findfirst: boolean; var sw: Word): AnsiString;
    function   GetData(id: AnsiString; var sw: Word): AnsiString;
    function   InternalAuthenticate(data: AnsiString; var sw: Word): AnsiString;
    function   ExternalAuthenticate(data: AnsiString; var sw: Word): AnsiString;
    function   GetChallenge(var sw: Word): AnsiString;
    procedure  DoOnTerminateWatcher(Sender: TObject);
  published
    property ActiveReaderIndex  : integer        read FUseReaderNum       write SetReaderNum  default -1;
    property APDULogging        : boolean        read FAPDULogging        write FAPDULogging  default false;

    property OnCardInserted:     TNotifyEvent    read FOnCardInserted     write FOnCardInserted;
    property OnCardActive:       TNotifyEvent    read FOnCardActive       write FOnCardActive;
    property OnCardRemoved:      TNotifyEvent    read FOnCardRemoved      write FOnCardRemoved;
    property OnCardInvalid:      TNotifyEvent    read FOnCardInvalid      write FOnCardInvalid;
    property OnReaderWaiting:    TNotifyEvent    read FOnReaderWaiting    write FOnReaderWaiting;
    property OnReaderListChange: TNotifyEvent    read FOnReaderListChange write FOnReaderListChange;
    property OnError:            TPCSCErrorEvent read FOnError            write FOnError;
    property OnCHVNeeded:        TPCSCPinEvent   read FOnCHVNeeded        write FOnCHVNeeded;

    property ReaderList:         TStringList     read FReaderList;
    property NumReaders:         integer         read FNumReaders;
    property CardConnected:      boolean         read FConnected;
    property Opened:             boolean         read IsReaderOpen;
    property ReaderState:        cardinal        read GetReaderState;
    property AttrProtocol:       integer         read FAttrProtocol;
    property AttrICCType:        string          read FAttrICCType;
    property AttrCardATR:        AnsiString      read FAttrCardATR;
    property AttrATRHistBytes:   string          read GetAttrATRHistBytes;
    property AttrATR:            ATRRec          read GetATR;
    property AttrVendorName:     string          read FAttrVendorName;
    property AttrVendorSerial:   string          read FAttrVendorSerial;
    property AttrInterfaceStatus: integer        read FAttrInterfaceStatus;
    property AttrDefaultDataRate: integer        read FAttrDefaultDataRate;
    property AttrDefaultCLK     : integer        read FAttrDefaultCLK;
  end;

procedure Register;

implementation

var
  ActReaderState  : cardinal;
  LastReaderState : cardinal;
  SelectedReader  : PWideChar;
  ReaderOpen      : boolean;

const
  GCGetResponse:AnsiString = #$00#$C0#$00#$00;

procedure Register;
begin
  RegisterComponents('More...', [TPCSCConnector]);
end;

function OrdD(const From: string; const Index: integer): integer;
begin
  if Index <= Length(From) then
     Result := Ord(From[Index])
  else Result := 0;
end;

procedure TPCSCConnector.MessageWndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_CARDSTATE) then
  begin
    if Msg.WParam <> SCARD_S_SUCCESS then
    begin
      if Assigned(FOnError) then
        FOnError(Self, esGetStatus, Msg.WParam);
    end;

    if FActReaderState <> FLastReaderState then
    begin
      ProcessReaderState(FLastReaderState, FActReaderState);
    end;
  end else
    Msg.Result := DefWindowProc(FNotifyHandle, Msg.Msg, Msg.WParam, Msg.LParam);
end;

procedure TPCSCConnector.ClearReaderAttributes;
begin
  FAttrCardATR      := '';
  FAttrVendorName   := '';
  FAttrVendorSerial := '';
  FAttrProtocol     := 0;
  FAttrICCType      := '';
end;

constructor TPCSCConnector.Create (AOwner: TComponent);
begin
  inherited Create(AOwner);
  FReaderWatcher := nil;
  FNotifyHandle := 0;
  FReaderList   := TStringlist.Create;
  FContext      := 0;
  FCardHandle   := 0;
  FNumReaders   := 0;
  FUseReaderNum := -1;
  FConnected    := false;
  FAPDULogging  := false;
  ActReaderState  := SCARD_STATE_UNAWARE;
  LastReaderState := SCARD_STATE_UNAWARE;
  ReaderOpen      := false;
  ClearReaderAttributes;
end;

destructor TPCSCConnector.Destroy;
begin
  CloseAndDisconnect;
  SCardReleaseContext(FContext);
  FReaderList.Free;
  if not (csDesigning in ComponentState) then
     DeallocateHWnd(FNotifyHandle);
  inherited Destroy;
end;

procedure TPCSCConnector.ReleaseContext;
begin
  if SCardIsValidContext(FContext) = SCARD_S_SUCCESS then
  begin
    SCardReleaseContext(FContext);
  end;

  FContext := 0;
end;

function TPCSCConnector.ConnectCardInSelectedReader: boolean;
var
  RetVar : cardinal;
begin
  RetVar := SCardConnectW(FContext, PChar(GetSelectedReaderName), SCARD_SHARE_EXCLUSIVE, SCARD_PROTOCOL_Tx, FCardHandle, @FAttrProtocol);
  case RetVar of
    SCARD_S_SUCCESS:
      begin
        CardActiveAction;
        Result := true;
      end;

    SCARD_W_REMOVED_CARD:
      begin
        CardRemovedAction;
        Result := true;
      end;

    else begin
      Result := false;

      if Assigned(FOnError) then
        FOnError(Self, esConnect, RetVar);
    end;
  end;
end;

function TPCSCConnector.Init: boolean;
var
  RetVar: cardinal;
  ReaderList: string;
  ReaderListSize: integer;
  i: integer;
begin
  Result := false;

  if (not (csDesigning in ComponentState)) and (FNotifyHandle = 0) then
    FNotifyHandle := AllocateHWnd(MessageWndProc);

  CloseAndDisconnect;
  ReleaseContext;

  RetVar := SCardEstablishContext(SCARD_SCOPE_USER, nil, nil, @FContext);

  if RetVar = SCARD_S_SUCCESS then
  begin
    ReaderListSize := 0;
    RetVar := SCardListReadersW(FContext, nil, nil, ReaderListSize);

    if RetVar = SCARD_S_SUCCESS then
    begin
      SetLength(ReaderList, ReaderListSize);
      SCardListReadersW(FContext, nil, PChar(ReaderList), ReaderListSize);

      FReaderList.Delimiter := #0;
      FReaderList.StrictDelimiter := true;
      FReaderList.DelimitedText := ReaderList;

      for i := FReaderList.Count-1 downto 0 do
      begin
        if FReaderList[i] = '' then
          FReaderList.Delete(i);
      end;

      if FReaderList.Count > 0 then
      begin
        if Assigned(FOnReaderListChange) then
          FOnReaderListChange(Self);

        Result := true;
      end;
    end else
    if Assigned(FOnError) then
      FOnError(Self, esInit, RetVar);
  end else
  if Assigned(FOnError) then
    FOnError(Self, esInit, RetVar);
end;


function TPCSCConnector.InternalAuthenticate(data: AnsiString; var sw: Word): AnsiString;
var
  len: integer;
begin
  Result := '';
  len := length(data);
  if len > $FF then exit;

  Result := data;
  if not GetResponseFromCard(#$00#$88#$00#$00 + AnsiChar(len), Result, sw)
  then
    Result := '';
end;

function TPCSCConnector.Open: boolean;
var
  ThreadID    : LongWord;
begin
  if FNotifyHandle = 0 then
    raise Exception.Create('Not initialized yet!');
  CloseAndDisconnect;

  ConnectCardInSelectedReader;

  if (FUseReaderNum > NOREADERSELECTED) and
     (SCardIsValidContext(FContext) = SCARD_S_SUCCESS) then
    begin
      ReaderOpen      := true;
      ActReaderState  := SCARD_STATE_UNAWARE;
      LastReaderState := SCARD_STATE_UNAWARE;
    // BeginThread(nil, 0, CardWatcherThread, @FContext, 0, ThreadID);
      FReaderWatcher := TReaderWatcher.Create(self);
      FReaderWatcher.OnTerminate := DoOnTerminateWatcher;
      FReaderWatcher.Suspended := false;

      Result := true;
    end else
    Result := false;
end;

procedure TPCSCConnector.Close;
begin
  ReaderOpen := false;
  sleep(10);
  SCardCancel(FContext);
  if FConnected then
  begin
    if Assigned(FReaderWatcher) then
    begin
      FReaderWatcher.Terminate;
      FReaderWatcher := nil;
    end;
    DisconnectCard;
  end;
end;

function TPCSCConnector.ConnectCard: boolean;
begin
  if FConnected then DisconnectCard;
  if FUseReaderNum > NOREADERSELECTED then
    if ConnectSelectedReader then FConnected := true
                             else FConnected := false;
  Result := FConnected;
end;

procedure TPCSCConnector.DisconnectCard;
begin
  if FConnected then
  begin
    FConnected  := false;
    Sleep(100);
    SCardDisconnect(FCardHandle, SCARD_RESET_CARD);
    FCardHandle := 0;
  end;
end;

procedure TPCSCConnector.DoOnTerminateWatcher(Sender: TObject);
begin
  if FReaderWatcher = Sender then
    FReaderWatcher := nil;

end;

function TPCSCConnector.ExternalAuthenticate(data: AnsiString;
  var sw: Word): AnsiString;
var
  len: integer;
begin
  Result := '';
  len := length(data);
  if len > $FF then exit;

  Result := data;
  if not GetResponseFromCard(#$00#$82#$00#$00 + AnsiChar(len), Result, sw)
  then
    Result := '';
end;

procedure TPCSCConnector.CloseAndDisconnect;
begin
  if FConnected then
     DisconnectCard;

  if ReaderOpen then
     Close;
end;

function TPCSCConnector.ConnectSelectedReader: boolean;
var
  RetVar : cardinal;
begin
  RetVar := SCardConnectW(FContext, SelectedReader, SCARD_SHARE_SHARED, SCARD_PROTOCOL_Tx, FCardHandle, @FAttrProtocol);
  case RetVar of
    SCARD_S_SUCCESS      : begin
                             CardActiveAction;
                             Result := true;
                           end;
    SCARD_W_REMOVED_CARD : begin
                             Result := true;
                           end;
    else                   begin
                             Result := false;
                             if Assigned(FOnError) then
                                FOnError(Self, esConnect, RetVar);
                           end;
    end;
end;

procedure TPCSCConnector.ProcessReaderState(const OldState, NewState: cardinal);
var
  CardInOld, CardInNew     : boolean;
  ReaderEmOld, ReaderEmNew : boolean;
  CardMuteOld, CardMuteNew : boolean;
  CardIgnore               : boolean;
begin
  CardInOld   := (OldState and SCARD_STATE_PRESENT) > 0;
  CardInNew   := (NewState and SCARD_STATE_PRESENT) > 0;
  ReaderEmOld := (OldState and SCARD_STATE_EMPTY) > 0;
  ReaderEmNew := (NewState and SCARD_STATE_EMPTY) > 0;
  CardMuteOld := (OldState and SCARD_STATE_MUTE) > 0;
  CardMuteNew := (NewState and SCARD_STATE_MUTE) > 0;
  CardIgnore  := (NewState and SCARD_STATE_IGNORE) > 0;

  FLastReaderState := NewState;

  if (CardMuteNew and not CardMuteold) and Assigned(FOnCardInvalid) then
    FOnCardInvalid(Self);

  if CardInNew and (not CardInOld) and (not CardMuteNew) and (not CardIgnore) then
    CardInsertedAction;

  if CardInOld and not CardInNew then
    CardRemovedAction;

  if ReaderEmNew and not ReaderEmOld and Assigned(FOnReaderWaiting) then
  begin
    FOnReaderWaiting(Self);
  end;
end;

procedure TPCSCConnector.CardInsertedAction;
begin
  if Assigned(FOnCardInserted) then FOnCardInserted(Self);
  if FConnected then CardActiveAction;
end;

procedure TPCSCConnector.CardActiveAction;
begin
  GetReaderAttributes;
  if FAttrProtocol <> SCARD_PROTOCOL_UNK then
    begin
      if Assigned(FOnCardActive) then
         FOnCardActive(Self);
    end;
end;

procedure TPCSCConnector.CardRemovedAction;
begin
  ClearReaderAttributes;
  if Assigned(FOnCardRemoved) then
  FOnCardRemoved(Self);

  DisconnectCard;
end;

// EMV 4.3 book 1 §11.3 page 127
function TPCSCConnector.CardSelect(const aid: AnsiString; findfirst: boolean; var sw: Word): AnsiString;
var
  p2: AnsiChar;
begin
  Result := '';
  if length(aid) > 250 then exit;
  p2 := #$00;
  Result := AnsiChar(byte(length(aid))) + aid;
  // p1=0x04 - select by name
  // p2=0x00 - select First or only occurrence
  // p2=0x02 - select Next occurrence
  if not findfirst then p2 := #$02;
  if GetResponseFromCard(#$00#$A4#$04 + p2, Result, sw) <> true then
    Result := '';
end;

procedure TPCSCConnector.SetReaderNum(Value: Integer);
begin
  if Value <> FUseReaderNum then
    begin
    CloseAndDisconnect;
    if Value < FReaderList.Count then
      begin
        SelectedReader := PChar(FReaderList[Value]);
        FUseReaderNum   := Value;
      end else
      begin
        SelectedReader := '';
        FUseReaderNum   := -1;
      end;
    end;
end;

function TPCSCConnector.IsReaderOpen: boolean;
begin
  Result := ReaderOpen;
end;

function TPCSCConnector.GetReaderState: cardinal;
begin
  Result := ActReaderState;
end;

procedure TPCSCConnector.GetReaderAttributes;
var
  RetVar : cardinal;
  ABuf   : AnsiString;
  AIBuf  : integer;
  ALen   : integer;
begin
  ABuf := StringOfChar(#0, 127);
  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_ATR_STRING, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrCardATR := Copy(ABuf, 1, ALen)
                              else FAttrCardATR := '';

  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_VENDOR_NAME, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrVendorName := Copy(ABuf, 1, ALen)
                              else FAttrVendorName := '';

  ALen := Length(ABuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_VENDOR_IFD_SERIAL_NO, Pointer(ABuf), @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrVendorSerial := Copy(ABuf, 1, ALen)
                              else FAttrVendorSerial := '';

  ALen := SizeOf(AIBuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_CURRENT_PROTOCOL_TYPE, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrProtocol := AIBuf
                              else FAttrProtocol := 0;

  ALen := SizeOf(AIBuf);
  AIBuf := 0;
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_ICC_TYPE_PER_ATR, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then begin
                                   case AIBuf of
                                     1  : FAttrICCType := 'ISO7816 Async';
                                     2  : FAttrICCType := 'ISO7816 Sync';
                                     else FAttrICCType := 'UNKNOWN';
                                     end;
                                   end
                              else FAttrICCType := '';

  ALen := SizeOf(AIBuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_ICC_INTERFACE_STATUS, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrInterfaceStatus := AIBuf
                              else FAttrInterfaceStatus := 0;

  ALen := SizeOf(AIBuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_DEFAULT_DATA_RATE, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrDefaultDataRate := AIBuf
                              else FAttrDefaultDataRate := 0;

  ALen := SizeOf(AIBuf);
  RetVar := SCardGetAttrib(FCardHandle, SCARD_ATTR_DEFAULT_CLK, @AIBuf, @ALen);
  if RetVar = SCARD_S_SUCCESS then FAttrDefaultCLK := AIBuf
                              else FAttrDefaultCLK := 0;


end;

function TPCSCConnector.GetATR: ATRRec;
begin
  Result.Clear;
  if Length(FAttrCardATR) < 2 then exit;

  Result.Load(FAttrCardATR);
end;

function TPCSCConnector.GetAttrATRHistBytes: string;
var
  ATR: ATRrec;
begin
  Result := '';
  if Length(FAttrCardATR) < 2 then exit;

  ATR.Load(FAttrCardATR);
  if ATR.Valid then Result := ATR.HistoricalBytes;
end;

// EMV book 3, section 6.5.6 GET CHALLENGE Command-Response APDUs
function TPCSCConnector.GetChallenge(var sw: Word): AnsiString;
begin
  Result := '';
  if not GetResponseFromCard(#$00#$84#$00#$00#$00, Result, sw)
  then
    Result := '';

  if Hi(sw) = $6C then
  begin
    Result := '';
    if not GetResponseFromCard(
          #$00#$84#$00#$00 + AnsiChar(Lo(sw)), Result, sw)
    then
      Result := '';
  end;
end;

function TPCSCConnector.GetData(id: AnsiString; var sw: Word): AnsiString;
var
  res: AnsiString;
begin
  Result := '';
  if length(id) <> 2 then exit;

  res := '';
  if not GetResponseFromCard(
        #$80#$CA + id[1] + id[2] + #$00, res, sw)
  then
    res := '';

  if Hi(sw) = $6C then
  begin
    res := '';
    if not GetResponseFromCard(
          #$80#$CA + id[1] + id[2] + AnsiChar(Lo(sw)), res, sw)
    then
      res := '';
  end;

  if (length(res) < 3) or (res[1] <> id[1]) or (res[2] <> id[2]) then exit;

  Result := res;
end;

procedure TPCSCConnector.ClearCardAttributes;
begin
  FGSMCurrentFile := '';
  FGSMFileInfo    := '';
  FGSMDirInfo     := '';
  FGSMVoltage30   := false;
  FGSMVoltage18   := false;
end;

function TPCSCConnector.GetResponseFromCard(const APdu: AnsiString): AnsiString;
var
  RetVar : cardinal;
  SBuf   : AnsiString;
  SLen   : cardinal;
  RBuf   : AnsiString;
  RLen   : cardinal;
  Ppci   : Pointer;
begin
  SBuf := apdu;
  if FAPDULogging then AddLog('--> ' + Bin2Hex(apdu));
  RBuf := AnsiString(StringOfChar(#0, MAXAPDULENGTH));
  if Length(SBuf) <= MAXAPDULENGTH then
  begin
    case FAttrProtocol of
      SCARD_PROTOCOL_T0: Ppci := @SCARD_PCI_T0;
      SCARD_PROTOCOL_T1: Ppci := @SCARD_PCI_T1;
    else Ppci := nil;
    end;

    SLen := Length(apdu);
    RLen := Length(RBuf);
    RetVar := SCardTransmit(FCardHandle, Ppci, @SBuf[1], SLen, nil, @RBuf[1], @RLen);
    if RetVar = SCARD_S_SUCCESS then
    begin
      Result := Copy(RBuf, 1, RLen);
      if FAPDULogging then AddLog('<-- ' + Bin2Hex(Result));
    end else
    begin
      if FAPDULogging then AddLog('<-- error: ' + IntToHex(RetVar, 8));
      Result := '';
      if Assigned(FOnError) then FOnError(Self, esTransmit, RetVar);
    end;
  end;
end;

function TPCSCConnector.GetResponseFromCard(const Command: AnsiString; var Data: AnsiString; var sw: word): boolean;
var
  Answer  : AnsiString;
  AnswerL : integer;
begin
  sw := 0;
  Answer := GetResponseFromCard(Command + Data);
  AnswerL := Length(Answer);
  if AnswerL >= 2 then
  begin
    Data := Copy(Answer, 1, AnswerL - 2);
    sw  := Byte(Answer[AnswerL - 1]) shl 8;
    sw  := sw or Byte(Answer[AnswerL]);
    if Hi(sw) = $61 then
    begin
      Data := AnsiChar(sw and $FF);
      if not GetResponseFromCard(GCGetResponse, Data, sw) then
      begin
        Data := '';
        sw  := 0;
        Result := false;
      end else
      Result := true;
    end else
    Result := true;
  end else
  begin
    Data := '';
    sw  := 0;
    Result := false;
  end;
end;

function TPCSCConnector.GetSelectedReaderName: String;
begin
  result := FReaderList[FUseReaderNum];
end;

{ TReaderWatcher }

constructor TReaderWatcher.Create(AOwner: TPCSCConnector);
begin
  inherited Create(true);
  FOwner := AOwner;
end;

procedure TReaderWatcher.Execute;
var
  RetVar   : cardinal;
  RStates  : array[0..1] of SCARD_READERSTATEW;
  SelReader: String;
begin
  FreeOnTerminate := true;

  SelReader := FOwner.GetSelectedReaderName;

  ZeroMemory(@RStates[0], SizeOf(SCARD_READERSTATEW));
  RStates[0].szReader     := PChar(SelReader);
  RStates[0].pvUserData   := nil;
  RStates[0].dwEventState := FOwner.FActReaderState;

  while (not Terminated) and FOwner.IsReaderOpen do
  begin
    sleep(100);
    if (SCardIsValidContext(FOwner.FContext) <> SCARD_S_SUCCESS) then
    begin
      //RetVal := SCardEstablishContext(...);
      Exit;
    end;

    RetVar := SCardGetStatusChangeW(FOwner.FContext, 100, RStates, 1);

    if not Terminated then
    begin
      case RetVar of
        SCARD_E_TIMEOUT:;

        SCARD_S_SUCCESS:
          begin
           if ((RStates[0].dwEventState and SCARD_STATE_CHANGED) <> 0) then
           begin
            RStates[0].dwCurrentState := RStates[0].dwEventState xor SCARD_STATE_CHANGED;
            FOwner.FActReaderState := RStates[0].dwEventState;

            PostMessage(FOwner.FNotifyHandle, WM_CARDSTATE, RetVar, 0);
          end;
        end;
      end;
    end;
  end;
end;

{function CardWatcherThread(PContext: pointer): integer;
var
  RetVar   : cardinal;
  RContext : cardinal;
  RStates  : array[0..1] of SCARD_READERSTATEW;
begin
  try
    RContext := cardinal(PContext^);
    FillChar(RStates,SizeOf(RStates),#0);
    RStates[0].szReader     := SelectedReader;
    RStates[0].pvUserData   := nil;
    RStates[0].dwEventState := ActReaderState;
    while ReaderOpen do
    try
      RStates[0].dwCurrentState := RStates[0].dwEventState;
      RetVar := SCardGetStatusChangeW(RContext, 50, RStates, 1);
      if RetVar <> SCARD_E_TIMEOUT then
      begin
        ActReaderState := RStates[0].dwEventState;
        PostMessage(FNotifyHandle, WM_CARDSTATE, RetVar, 0);
      end;
    except
      break;
    end;
  finally
    Result := 0;
  end;
end;
 }


end.
