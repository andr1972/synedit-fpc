{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}

unit synedittextwrapper;

(*
This implementation is based on the TextTrimming classes.
Due to the architecture of SynEdit (which I don't fully understands nor like)
for this class to work properly, changes to CustomSynEdit, LineGutter, and
maybe others, are necessary.
-- Important remark: This is a very inefficient implementation, prone to
memory starvation. --
*)

{$I synedit.inc}

interface

uses
LCLProc,
  Classes, SysUtils, LazSynEditText, SynEditTextBase, SynEditTypes, SynEditHighlighter,
  SynEditPointClasses;

type

  TSynEditStringWrappingType = (setwVisual, setwReal);

  TSynEditStringWrappingList = class;

  { TLazSynDisplayWrap }

  TLazSynDisplayWrap = class(TLazSynDisplayViewEx)
  private
    FWrapper: TSynEditStringWrappingList;
  public
    constructor Create(AWrapper: TSynEditStringWrappingList);
    procedure SetHighlighterTokensLine(ALine: TLineIdx; out ARealLine: TLineIdx); override;
    function LineNumber(AIndex:TLineIdx):TLineIdx;override;
  end;

  { TSynEditStringWrappingList }
  TCharSet = set of char;

  TSynEditStringWrappingList = class(TSynEditStringsLinked)
  private
    fCaret: TSynEditCaret;
    fWordBreakingChars:TCharSet;
    FWrappingCount: integer;
    FWrapType: TSynEditStringWrappingType;
    fEnabled: Boolean;
    fLockCount: Integer;
    fMaxRealLine:integer;
    fRealLineNumber : array of integer; //visible lines with corresponding unwrapped line number
    FViewChangeStamp: int64;
    FDisplayView: TLazSynDisplayWrap;
    FCharsInWindow:integer; //cantidad de caracteres que caben en el espacio visible (tamaño máximo de línea)
    FMinCharsInW:integer;
    FSetCaretPos:TPoint;
    function getCharsInWindow: integer;
    procedure DoCaretChanged(Sender : TObject);
    function getIsWrapping: Boolean;
    function getMinCharsInW: integer;
    function getRealCount: integer;
    function getRealLine(Index: integer): string;
    function getWWC: TCharSet;
    procedure ListCleared(Sender: TObject);
    Procedure LinesChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    Procedure DoLineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    Procedure LineCountChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
    procedure setCharsInWindow(AValue: integer);
    procedure SetEnabled(const AValue : Boolean);
    procedure setMinCharsInW(AValue: integer);
    procedure SetWrapType(const AValue: TSynEditStringWrappingType);
    procedure setWWC(AValue: TCharSet);
    function  WrapLine(const S : String; Index: Integer) : string;
    function  JoinLine(const S : String; Index: Integer): string;
    procedure IncViewChangeStamp;
    function RemoveChar(s:string;cantidad,posicion:integer):string;
  protected
    function GetViewChangeStamp: int64; override;
    function  GetExpandedString(Index: integer): string; override;
    function  Get(Index: integer): string; override;
    procedure Put(Index: integer; const S: string); override;
    function GetDisplayView: TLazSynDisplayView; override;
    function joinStrings(s1,s2:string):string;virtual;
    function GetTextStr:string;override;
  public
    constructor Create(ASynStringSource: TSynEditStrings; ACaret: TSynEditCaret);
    destructor Destroy; override;

    function Add(const S: string): integer; override;
    procedure AddStrings(AStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure DeleteLines(Index, NumLines: integer);  override;
    procedure Insert(Index: integer; const S: string); override;
    procedure InsertLines(Index, NumLines: integer); override;
    procedure InsertStrings(Index: integer; NewStrings: TStrings); override;
    procedure Exchange(Index1, Index2: integer); override;
  public
    procedure Lock;
    procedure UnLock;
    property Enabled : Boolean read fEnabled write SetEnabled;

    property IsWrapping: Boolean read getIsWrapping;
    property WrapType: TSynEditStringWrappingType read FWrapType write SetWrapType;
    property CharsInWindow:integer read getCharsInWindow write setCharsInWindow;
    property MinCharsInWindow:integer read getMinCharsInW write setMinCharsInW;
    property WordBreakingChars:TCharSet read getWWC write setWWC;
    property RealCount:integer read getRealCount;
    property RealLine[Index:integer]:string read getRealLine;
    function RealLineNumber(ALine:TLineIdx):TLineIdx;
    procedure ReWrap;virtual;
    procedure UnWrap;virtual;
  public
    procedure EditInsert(LogX, LogY: Integer; AText: String); override;
    Function  EditDelete(LogX, LogY, ByteLen: Integer): String; override;
    procedure EditLineBreak(LogX, LogY: Integer); override;
    procedure EditLineJoin(LogY: Integer; FillText: String = ''); override;
    procedure EditLinesInsert(LogY, ACount: Integer; AText: String = ''); override;
    procedure EditLinesDelete(LogY, ACount: Integer); override;
    procedure EditUndo(Item: TSynEditUndoItem); override;
    procedure EditRedo(Item: TSynEditUndoItem); override;
  end;

implementation

{off $Define SynWrapUndoDebug}
{off $Define SynWrapDebug}
{$IFDEF SynUndoDebug}
  {$Define SynUndoDebugItems}
  {$Define SynWrapUndoDebug}
{$ENDIF}

{ TLazSynDisplayWrap }

constructor TLazSynDisplayWrap.Create(AWrapper: TSynEditStringWrappingList);
begin
  inherited Create;
  FWrapper := AWrapper;
end;

procedure TLazSynDisplayWrap.SetHighlighterTokensLine(ALine: TLineIdx; out ARealLine: TLineIdx);
begin
  CurrentTokenLine := ALine;
  inherited SetHighlighterTokensLine(ALine, ARealLine);
end;

function TLazSynDisplayWrap.LineNumber(AIndex: TLineIdx): TLineIdx;
begin
  result := inherited LineNumber(AIndex);
  if FWrapper.WrapType = setwVisual then
    Result:= FWrapper.RealLineNumber(result);
end;

{ TSynEditStringWrappingList }

constructor TSynEditStringWrappingList.Create(ASynStringSource : TSynEditStrings; ACaret: TSynEditCaret);
begin
  fCaret := ACaret;
  fCaret.AddChangeHandler(@DoCaretChanged);
  FDisplayView := TLazSynDisplayWrap.Create(Self);
  FDisplayView.NextView := ASynStringSource.DisplayView;
  fEnabled:=false;
  fMaxRealLine:= -1;
  FWrappingCount := 0;
  FWrapType := setwVisual;
  Inherited Create(ASynStringSource);
  fSynStrings.AddChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
  fSynStrings.AddChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LinesChanged);
  fSynStrings.AddNotifyHandler(senrCleared, {$IFDEF FPC}@{$ENDIF}ListCleared);
  MinCharsInWindow := 40;
  WordBreakingChars:= [];
end;

destructor TSynEditStringWrappingList.Destroy;
begin
  fSynStrings.RemoveChangeHandler(senrLineCount, {$IFDEF FPC}@{$ENDIF}LineCountChanged);
  fSynStrings.RemoveChangeHandler(senrLineChange, {$IFDEF FPC}@{$ENDIF}LinesChanged);
  fSynStrings.RemoveNotifyHandler(senrCleared, {$IFDEF FPC}@{$ENDIF}ListCleared);
  fCaret.RemoveChangeHandler(@DoCaretChanged);
  FreeAndNil(FDisplayView);
  Finalize(fRealLineNumber);
  inherited Destroy;
end;

function TSynEditStringWrappingList.getCharsInWindow: integer;
begin
  result := FCharsInWindow + 2;
end;

procedure TSynEditStringWrappingList.DoCaretChanged(Sender : TObject);
var
  y:integer;
begin
  if (not fEnabled) then exit;
  if IsWrapping then
    exit;
  y := fCaret.LinePos;
  if (FSetCaretPos.x > 0) then begin
    fCaret.LineBytePos:=FSetCaretPos;
    FSetCaretPos := Point(0,0);
    exit;
  end;
  if y >= fSynStrings.Count then exit;
  if fRealLineNumber[y - 1] <> fRealLineNumber[y] then exit;
  if fCaret.BytePos <= Length(fSynStrings.ExpandedStrings[y - 1]) + 2 then exit;
  fCaret.LineBytePos := Point(1,y + 1);
end;

function TSynEditStringWrappingList.getIsWrapping: Boolean;
begin
  result := FWrappingCount > 0;
end;

function TSynEditStringWrappingList.getMinCharsInW: integer;
begin
  result := FMinCharsInW;
end;

function TSynEditStringWrappingList.getRealCount: integer;
begin
  if FWrapType = setwReal then
    result := Count
  else
    result := FMaxRealLine + 1;
end;

function TSynEditStringWrappingList.getRealLine(Index: integer): string;
var
  l,tl:integer;
begin
  if fWrapType = setWReal then
    result := fSynStrings[Index]
  else begin
    if (Index < 0) or (index > fMaxRealLine) then exit('');
    l := 0;
    while fRealLineNumber[l] < Index do Inc(l);
    result := fSynStrings[l];
    Inc(l);
    tl := High(fRealLineNumber);
    while (l <= tl) and (fRealLineNumber[l] = Index) do begin
      result := JoinStrings(result,fSynStrings[l]);
      Inc(l);
    end;
  end;
end;

function TSynEditStringWrappingList.getWWC: TCharSet;
begin
  result := FWordBreakingChars;
end;

procedure TSynEditStringWrappingList.ListCleared(Sender: TObject);
begin
  Finalize(fRealLineNumber);
  fMaxRealLine:= -1;
end;

procedure TSynEditStringWrappingList.LinesChanged(Sender: TSynEditStrings; AIndex, ACount: Integer);
begin
  if (not fEnabled) then exit;
  if IsWrapping then exit;
  Inc(FWrappingCount);
  try
    while(ACount > 0) do begin
      Dec(ACount);
      fSynStrings[AIndex + ACount] := WrapLine(fSynStrings.ExpandedStrings[AIndex + ACount],AIndex + ACount);
    end;
  finally
    Dec(FWrappingCount);
  end;
end;

procedure TSynEditStringWrappingList.DoLineCountChanged(Sender: TSynEditStrings;
  AIndex, ACount: Integer);
var
  i, hrl,rn: Integer;
begin
//  if IsWrapping then exit;
  // resize the map to real lines, to accomodate current visual lines count
  IncViewChangeStamp;
  hrl := High(fRealLineNumber);
  if ACount < 0 then begin
    //lines had been deleted, if not at the end, then decrease line numbers acordingly
    if AIndex - ACount <= hrl then begin
       //if the last not deleted line, is the same real line as the first not deleted line after, nothing to do,
       // otherwise line numbers need to be decreased, taking into account the original "delta" among them
       if not IsWrapping and (fRealLineNumber[AIndex] <> fRealLineNumber[AIndex - ACount]) then begin
         //count line number changes in range
         rn := 0;
         for i:= AIndex to AIndex - ACount - 1 do begin
           if fRealLineNumber[i] <> fRealLineNumber[i + 1] then
             Inc(rn);
         end;
         for i := AIndex - ACount to hrl do
            Dec(fRealLineNumber[i],rn);
       end;
       System.Move(fRealLineNumber[AIndex - ACount],fRealLineNumber[AIndex],(hrl - AIndex - ACount-1) * sizeof(integer));
    end;
  end;
  if High(fRealLineNumber) < 0 then
    SetLength(fRealLineNumber,fSynStrings.Count)
  else
    SetLength(fRealLineNumber,hrl + ACount + 1);
  if ACount > 0 then begin
    if AIndex <= hrl then begin
      //move line numbers to the corresponding new line
      for i := High(fRealLineNumber) downto AIndex + ACount do
        fRealLineNumber[i] := fRealLineNumber[i-ACount];
    end;
    if AIndex > 0 then begin
      rn := fRealLineNumber[AIndex - 1];
      if not IsWrapping then
        Inc(rn);
    end else
      rn := 0;
    for i := AIndex to AIndex + ACount - 1 do begin
      fRealLineNumber[i] := rn;
      Inc(rn);
    end;
    //set new lines numbers if not Wrapping
    if not IsWrapping then begin
      for i := AIndex + ACount to High(fRealLineNumber) do
        Inc(fRealLineNumber[i],ACount);
    end;
  end;
  hrl := High(fRealLineNumber);
  if hrl >= 0 then
    fMaxRealLine:=fRealLineNumber[hrl]
  else
    fMaxRealLine:= -1;
end;

procedure TSynEditStringWrappingList.LineCountChanged(Sender: TSynEditStrings;
  AIndex, ACount: Integer);
begin
  if fEnabled then DoLineCountChanged(Sender, AIndex, ACount);
end;

procedure TSynEditStringWrappingList.setCharsInWindow(AValue: integer);
var
  pv,l:integer;
begin
  if AValue < MinCharsInWindow then
    AValue := MinCharsInWindow;
  if FCharsInWindow = AValue - 2 then exit;
  pv := FCharsInWindow;
  FCharsInWindow:= AValue - 2;
  if IsWrapping then
    exit;
  //recalcular todas las líneas
  Inc(FWrappingCount);
  try
    l := 0;
    if pv < FCharsInWindow then begin
      while l < fSynStrings.Count do begin
        fSynStrings[l] := JoinLine(fSynStrings.ExpandedStrings[l],l);
        Inc(l);
      end;
    end else begin
      while l < fSynStrings.Count do begin
        fSynStrings[l] := WrapLine(fSynStrings.ExpandedStrings[l],l);
        Inc(l);
      end;
    end;
  finally
    Dec(FWrappingCount);
  end;
end;

procedure TSynEditStringWrappingList.SetEnabled(const AValue : Boolean);
var
  l:integer;
begin
  if fEnabled = AValue then exit;
  fEnabled:=AValue;
  fLockCount:=0;
  if not fEnabled then
    UnWrap
  else
    reWrap;
end;

procedure TSynEditStringWrappingList.setMinCharsInW(AValue: integer);
begin
  FMinCharsInW := AValue;
  if AValue > CharsInWindow then
    CharsInWindow := AValue;
end;

procedure TSynEditStringWrappingList.SetWrapType(const AValue: TSynEditStringWrappingType);
begin
  if FWrapType = AValue then exit;
  FWrapType := AValue;
end;

procedure TSynEditStringWrappingList.setWWC(AValue: TCharSet);
begin
 if (AValue <> []) and (AValue = FWordBreakingChars) then exit;
 if AValue = [] then
   fWordBreakingChars := [' ',#9,#13,#10,',','.','-','(',')']
 else
   FWordBreakingChars := AValue;
end;

function TSynEditStringWrappingList.WrapLine(const S: String; Index: Integer): string;
var
  l, i,rn:integer;
begin
  if (not fEnabled) then exit(s);
    {$IFDEF SynWrapDebug}debugln(['--- Wrapper -- WrapLine ', ' fLineIndex=', fLineIndex, ' fSpaces=',length(fSpaces), '  RealUndo=', RealUndo ]);{$ENDIF}
  l := length(s);
  if l <= FCharsInWindow then
     exit(S);
  Inc(FWrappingCount);
  try
    i := FCharsInWindow;
    while (i > 0) and not (s[i] in fWordBreakingChars) do Dec(i);
    if i = 0 then begin
      i := FCharsInWindow;
      while (i < l) and not (s[i] in fWordBreakingChars) do Inc(i);
    end;
    if i < l then begin
      result := copy(s, 1, i);
      if Index + 1 < fSynStrings.Count then begin
        if fRealLineNumber[Index] = fRealLineNumber[Index+1] then
         fSynStrings[Index+1] := WrapLine(JoinStrings(copy(s,i + 1,l-i),fSynStrings.ExpandedStrings[Index+1]),Index + 1)
        else begin
          //line maybe wrapped again, so insert an empty one first
          fSynStrings.Insert(Index+1,'');
          fSynStrings[Index+1] :=  WrapLine(copy(s,i + 1,l-i),Index + 1);
        end;
      end
      else begin
          //line maybe wrapped again, so add an empty one first
          fSynStrings.Add('');
          fSynStrings[Index + 1] := WrapLine(copy(s,i + 1,l-i),Index + 1);
      end;
    end else
      result := s;
  finally
    Dec(FWrappingCount);
  end;
end ;

function TSynEditStringWrappingList.JoinLine(const S: String; Index: Integer
  ): string;
var
  maxnl,nll,l, i:integer;
  nl:string;
begin
  if (not fEnabled) then exit(s);
  if Index >= fSynStrings.Count - 1 then //nothing to join
     exit(s);
  if fRealLineNumber[Index] <> fRealLineNumber[Index + 1] then //no joinable lines
     exit(s);
  l := length(s);
  if l >= FCharsInWindow then //no space left to join any word
     exit(S);
  Inc(FWrappingCount);
  try
    //line remaining space
    nll := FCharsInWindow - l;
    //next line, from where to take words
    nl := fSynStrings.ExpandedStrings[Index + 1];
    l := Length(nl);
    maxnl := 0;
    i := 1;
    repeat
      while (i <= l) and not (nl[i] in fWordBreakingChars) do Inc(i);
      if i <= nll then
        maxnl := i;
      Inc(i);
    until (i >= l) or (i >= nll);
    if maxnl > 0 then begin
      //found something to join from the next line
      //check that exists a breaking char between the joining sections
      result := JoinStrings(s,Copy(nl, 1, maxnl));
      fSynStrings[Index + 1] := JoinLine(Copy(nl, maxnl + 1,l-maxnl),Index + 1);
      if fSynStrings.ExpandedStrings[Index + 1] = '' then
        //mapping to real lines is handled on LineCountChanged, no need to doit here (DON'T)
        fSynStrings.Delete(Index + 1);
    end else
      result := s;
  finally
    Dec(FWrappingCount);
  end;
end;

procedure TSynEditStringWrappingList.Lock;
begin
  inc(fLockCount);
end;

procedure TSynEditStringWrappingList.UnLock;
begin
  dec(fLockCount);
  if (FWrapType = setwVisual) then DoCaretChanged(fCaret);
end;

function TSynEditStringWrappingList.RealLineNumber(ALine: TLineIdx): TLineIdx;
begin
  if (ALine <= 0) or (ALine - 1 > High(fRealLineNumber)) then
    result := ALine
  else
    result := fRealLineNumber[ALine-1] + 1;
end;

procedure TSynEditStringWrappingList.ReWrap;
var
  l, hrl:integer;
begin
  //foces wrapping and joining of every line
  Inc(FWrappingCount);
  try
    if High(fRealLineNumber) < 0 then begin
      SetLength(fRealLineNumber,fSynStrings.Count);
      for l:= 0 to High(fRealLineNumber) do
        fRealLineNumber[l] := l;
      hrl := High(fRealLineNumber);
      if hrl >= 0 then
        fMaxRealLine:=fRealLineNumber[hrl]
      else
        fMaxRealLine:= -1;
    end;
    l := 0;
    while l < fSynStrings.Count do begin
      fSynStrings[l] := JoinLine(fSynStrings.ExpandedStrings[l],l);
      Inc(l);
    end;
    l := 0;
    while l < fSynStrings.Count do begin
      fSynStrings[l] := WrapLine(fSynStrings.ExpandedStrings[l],l);
      Inc(l);
    end;
  finally
    Dec(FWrappingCount);
  end;
end;

procedure TSynEditStringWrappingList.UnWrap;
var
  st:TStrings;
  l:integer;
begin
  //undo wrapping, restoring lines to real lines
  st := TStringList.Create;
  try
     for l:=0 to RealCount-1 do
       st.Add(RealLine[l]);
     fSynStrings.Assign(st);
     DoLineCountChanged(fSynStrings, 0, fSynStrings.Count);
  finally
    st.Free;
  end;
end;

// Lines
function TSynEditStringWrappingList.GetExpandedString(Index : integer) : string;
begin
  Result:= fSynStrings.ExpandedStrings[Index];
end;

function TSynEditStringWrappingList.Get(Index : integer) : string;
begin
  Result:= fSynStrings.Strings[Index];
end;

procedure TSynEditStringWrappingList.Put(Index : integer; const S : string);
begin
  fSynStrings.Strings[Index]:= WrapLine(S, Index);
end;

function TSynEditStringWrappingList.Add(const S : string) : integer;
begin
  Result := inherited Add(S);
  fSynStrings[Result] := WrapLine(S, result);
end;

procedure TSynEditStringWrappingList.AddStrings(AStrings : TStrings);
var
  i: Integer;
begin
  i := fSynStrings.Count;
  inherited AddStrings(AStrings);
  while i < fSynStrings.Count do begin
    fSynStrings[i] := WrapLine(fSynStrings.ExpandedStrings[i], i);
    Inc(i);
  end;
end;

procedure TSynEditStringWrappingList.Clear;
begin
  fSynStrings.Clear;
  Finalize(fRealLineNumber);
  fMaxRealLine:=-1;
end;

procedure TSynEditStringWrappingList.Delete(Index : integer);
begin
  fSynStrings.Delete(Index);
  System.Move(fRealLineNumber[Index + 1],fRealLineNumber[Index],(High(fRealLineNumber) - Index) * sizeof(integer));
  SetLength(fRealLineNumber,High(fRealLineNumber));
  if High(fRealLineNumber) >= 0 then
    fMaxRealLine:=fRealLineNumber[High(fRealLineNumber)]
  else
    fMaxRealLine:=-1;
end;

procedure TSynEditStringWrappingList.DeleteLines(Index, NumLines : integer);
begin
  fSynStrings.DeleteLines(Index, NumLines);
  System.Move(fRealLineNumber[Index + NumLines],fRealLineNumber[Index],(High(fRealLineNumber) - Index - NumLines) * sizeof(integer));
  SetLength(fRealLineNumber,High(fRealLineNumber) - NumLines);
  if High(fRealLineNumber) >= 0 then
    fMaxRealLine:=fRealLineNumber[High(fRealLineNumber)]
  else
    fMaxRealLine:=-1;
end;

procedure TSynEditStringWrappingList.Insert(Index : integer; const S : string);
begin
  fSynStrings.Insert(Index, WrapLine(S, Index));
end;

procedure TSynEditStringWrappingList.InsertLines(Index, NumLines : integer);
begin
  fSynStrings.InsertLines(Index, NumLines);
end;

procedure TSynEditStringWrappingList.InsertStrings(Index : integer; NewStrings : TStrings);
var
  i : Integer;
begin
  for i := 0 to NewStrings.Count-1 do
    NewStrings[i] := WrapLine(NewStrings[i], Index+i);
  fSynStrings.InsertStrings(Index, NewStrings);
end;

function TSynEditStringWrappingList.GetDisplayView: TLazSynDisplayView;
begin
  Result := FDisplayView;
end;

function TSynEditStringWrappingList.joinStrings(s1, s2: string): string;
begin
  //make sure there is a WordBreakingChar between strings
  if (s2 <> '') and (s1 <> '') and not(s2[1] in fWordBreakingChars) and not(s1[Length(s1)] in fWordBreakingChars) then
    result := s1 + ' ' + s2
  else
    result := s1 + s2;
end;

function TSynEditStringWrappingList.GetTextStr: string;
var
  l:integer;
begin
  if FWrapType = setwReal then
    Result:=inherited GetTextStr
  else begin
    result := RealLine[0];
    for l := 1 to RealCount - 1 do
      result := result + LineEnding + RealLine[l];
  end;
end;

procedure TSynEditStringWrappingList.Exchange(Index1, Index2 : integer);
begin
  fSynStrings.Exchange(Index1, Index2);
end;

procedure TSynEditStringWrappingList.IncViewChangeStamp;
begin
  {$PUSH}{$Q-}{$R-}
  FViewChangeStamp := FViewChangeStamp + 1;
  {$POP}
end;

function TSynEditStringWrappingList.RemoveChar(s: string; cantidad,
  posicion: integer): string;
begin
  if fSynStrings.IsUtf8 then begin
    while cantidad > 0 do begin
      if not(s[posicion] in [#$80..#$BF]) then
        Dec(cantidad);
      System.Delete(s,posicion,1);
    end;
  end else
    System.Delete(s,posicion,cantidad);
  result := s;
end;

function TSynEditStringWrappingList.GetViewChangeStamp: int64;
begin
  Result := inherited GetViewChangeStamp;
  {$PUSH}{$Q-}{$R-}
  Result := Result + FViewChangeStamp;
  {$POP}
end;

procedure TSynEditStringWrappingList.EditInsert(LogX, LogY: Integer; AText: String);
var
  t: String;
  Len:integer;
begin
  Inc(FWrappingCount);
  try
    fSynStrings.EditInsert(LogX, LogY, AText);
    if (not fEnabled) then
      exit;

    IncIsInEditAction;
    t := fSynStrings.ExpandedStrings[LogY - 1]; //get white chars too
    Len := length(t);
    if Len > FCharsInWindow then begin
      t := WrapLine(t,LogY-1);
      fSynStrings[LogY - 1]  := t;
      if LogX > Length(t) then begin
        FSetCaretPos :=Point(LogX - Length(t) + 1,LogY + 1);
        fCaret.LineBytePos := FSetCaretPos;
      end;
    end;
    DecIsInEditAction;
  finally
    Dec(FWrappingCount);
  end;
end;

function TSynEditStringWrappingList.EditDelete(LogX, LogY, ByteLen: Integer): String;
var
  c,lc:integer;
  s:string;
begin
  result := fSynStrings.EditDelete(LogX, LogY, ByteLen);
  if (not fEnabled) then
    exit;

  IncIsInEditAction;
  FlushNotificationCache;
  // check for line joining, with previous line first, if deleting at first word
  if LogY > 1 then begin
    c := 1;
    s := fSynStrings[LogY - 1];
    lc := Length(s);
    while (c < lc) and not(s[c] in fWordBreakingChars) do Inc(c);
    if (LogX <= 1) or (c <= LogX) then begin
      //if lines get joined, then caret position must change
      fSynStrings[LogY - 2] := JoinLine(fSynStrings.ExpandedStrings[LogY-2],LogY-2);
      if (LogY > fSynStrings.Count) then begin
        FSetCaretPos.y := fSynStrings.Count;
        FSetCaretPos.x := Length(fSynStrings[FSetCaretPos.y - 1]) - LogX + c + 1;
        fCaret.LineBytePos:=FSetCaretPos;
      end else if (Copy(s,1,c) <> Copy(fSynStrings[LogY-1],1,c)) then begin
        FSetCaretPos := Point(LogX - c,LogY);
        if FSetCaretPos.x <=0 then
          FSetCaretPos := Point(Length(fSynStrings[LogY-2]) + FSetCaretPos.x,LogY-1);
        fCaret.LineBytePos:=FSetCaretPos;
      end;
    end;
  end;
  if LogY < fSynStrings.Count then
    fSynStrings[LogY - 1] := JoinLine(fSynStrings.ExpandedStrings[LogY-1],LogY-1);
  DecIsInEditAction;
end;

procedure TSynEditStringWrappingList.EditLineBreak(LogX, LogY: Integer);
begin
  fSynStrings.EditLineBreak(LogX, LogY);
  if (not fEnabled) then exit;
  if IsWrapping then exit;
  //add new real line, no need to wrap, may need to join
  if LogY < fSynStrings.Count - 1 then;
    fSynStrings[LogY] := JoinLine(fSynStrings.ExpandedStrings[LogY],LogY);
end;

procedure TSynEditStringWrappingList.EditLineJoin(LogY: Integer;
  FillText: String = '');
var
  s: String;
  hrn,rn:integer;
begin
  hrn := High(fRealLineNumber);
  if fEnabled and not IsWrapping then begin
    //check if this is about the same line, or different lines (real lines)
    if (LogY <= hrn) and (fRealLineNumber[LogY - 1] = fRealLineNumber[LogY]) then begin
      //same real line, becomes a character delete
      //I may be joining because deleting at end of LogY, or at beginning of LogY+1
      if fCaret.OldLinePos > LogY then begin
        s := fSynStrings.ExpandedStrings[LogY-1];
        if s <> '' then begin
          fSynStrings[LogY-1] := RemoveChar(s,1,Length(s));
          FSetCaretPos := Point(Length(s),LogY);
          fCaret.LineBytePos := FSetCaretPos;
          exit;
        end;
      end else begin
        s := fSynStrings.ExpandedStrings[LogY];
        if s <> '' then begin
          fSynStrings[LogY] := RemoveChar(s,1,1);
          FSetCaretPos := Point(1,LogY+1);
          fCaret.LineBytePos := FSetCaretPos;
          exit;
        end;
      end;
    end;
  end;
  //real line joining, don't wrap before deleting joining line
  Inc(FWrappingCount);
  try
    //if joining lines from different real lines, they become the same real line
    if (LogY <= hrn) and (fRealLineNumber[LogY-1] <> fRealLineNumber[LogY]) then begin
      for rn := LogY to hrn do
        Dec(fRealLineNumber[rn]);
    end;
    fSynStrings.EditLineJoin(LogY, FillText);
    fSynStrings[LogY-1] := WrapLine(FsynStrings.ExpandedStrings[LogY-1],LogY-1);
    //if new line wrapped, may need to join with next one
    fSynStrings[LogY] := JoinLine(FsynStrings.ExpandedStrings[LogY],LogY);
  finally
    Dec(FWrappingCount);
  end;
end;

procedure TSynEditStringWrappingList.EditLinesInsert(LogY, ACount: Integer;
  AText: String = '');
begin
  IncIsInEditAction;
  FlushNotificationCache;
  fSynStrings.EditLinesInsert(LogY, ACount, AText);
  DecIsInEditAction;
end;

procedure TSynEditStringWrappingList.EditLinesDelete(LogY, ACount: Integer);
begin
  IncIsInEditAction;
  FlushNotificationCache;
  fSynStrings.EditLinesDelete(LogY, ACount);
  DecIsInEditAction;
end;

procedure TSynEditStringWrappingList.EditUndo(Item: TSynEditUndoItem);
begin
  EditRedo(Item);
end;

procedure TSynEditStringWrappingList.EditRedo(Item: TSynEditUndoItem);
begin
  IncIsInEditAction; // all undo calls edit actions
  if not Item.PerformUndo(self) then
    inherited EditRedo(Item);
  DecIsInEditAction;
end;

end.

