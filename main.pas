//------------------------------------------------------------------------------
//
//  ENDEDIT: An ENDTEXT Editor
//  Copyright (C) 2021-2022 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Main Form
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/endedit/
//------------------------------------------------------------------------------

unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Buttons, Clipbrd, ExtDlgs, pngimage, xTGA, zBitmap,
  Menus, ImgList, jpeg, StdCtrls, ee_undo, ee_filemenuhistory, ee_screen,
  ee_textart;

const
  MINZOOM = 0;
  MAXZOOM = 20;

const
  MOUSEWHEELTIMEOUT = 100; // Msecs until next mouse wheel even to be proccessed

const
  COLORPICKPALETTESIZE = 24;
  CHARACTERPICKPALETTESIZE = 16;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    LeftPanel: TPanel;
    OpenSpeedButton1: TSpeedButton;
    PasteSpeedButton1: TSpeedButton;
    SaveSpeedButton1: TSpeedButton;
    CopySpeedButton1: TSpeedButton;
    GridButton1: TSpeedButton;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    N2: TMenuItem;
    Paste1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Timer1: TTimer;
    Panel4: TPanel;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    New1: TMenuItem;
    N3: TMenuItem;
    Save2: TMenuItem;
    Cut1: TMenuItem;
    HistoryItem0: TMenuItem;
    HistoryItem1: TMenuItem;
    HistoryItem2: TMenuItem;
    HistoryItem3: TMenuItem;
    HistoryItem4: TMenuItem;
    HistoryItem5: TMenuItem;
    HistoryItem6: TMenuItem;
    HistoryItem7: TMenuItem;
    HistoryItem8: TMenuItem;
    HistoryItem9: TMenuItem;
    N14: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    N4: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    NewSpeedButton1: TSpeedButton;
    UndoSpeedButton1: TSpeedButton;
    RedoSpeedButton1: TSpeedButton;
    ZoomOutSpeedButton1: TSpeedButton;
    ZoomInSpeedButton1: TSpeedButton;
    ToolPanel: TPanel;
    BackgroundPalettePanel1: TPanel;
    BackgroundPalette1: TImage;
    CursorBlinkTimer: TTimer;
    Panel5: TPanel;
    ForegroundPalettePanel1: TPanel;
    ForegroundPalette1: TImage;
    Panel6: TPanel;
    FreeDrawSpeedButton: TSpeedButton;
    FloodFillSpeedButton: TSpeedButton;
    RectSpeedButton: TSpeedButton;
    FillRectSpeedButton: TSpeedButton;
    LineSpeedButton: TSpeedButton;
    EraseTextSpeedButton: TSpeedButton;
    TextSpeedButton: TSpeedButton;
    EclipseSpeedButton: TSpeedButton;
    FilledEclipseSpeedButton: TSpeedButton;
    SpecialCharactersPanel1: TPanel;
    SpecialCharacters1: TImage;
    Panel3: TPanel;
    SpecialCharSpeedButton1: TSpeedButton;
    TextColorChangeSpeedButton: TSpeedButton;
    CharRect1SpeedButton: TSpeedButton;
    CharRect2SpeedButton: TSpeedButton;
    CharRect3SpeedButton: TSpeedButton;
    CharRect4SpeedButton: TSpeedButton;
    CharRect5SpeedButton: TSpeedButton;
    SpecialCharSpeedButton2: TSpeedButton;
    SpecialCharSelectSpeedButton1: TSpeedButton;
    SpecialCharSelectSpeedButton2: TSpeedButton;
    PaintBoxPopupMenu1: TPopupMenu;
    Undo2: TMenuItem;
    Redo2: TMenuItem;
    N5: TMenuItem;
    Cut2: TMenuItem;
    Copy2: TMenuItem;
    Paste2: TMenuItem;
    BlinkOnSpeedButton: TSpeedButton;
    BlinkOffSpeedButton: TSpeedButton;
    Export1: TMenuItem;
    N6: TMenuItem;
    SavePictureDialog1: TSavePictureDialog;
    Onlinedocumentation1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Open1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure File1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure New1Click(Sender: TObject);
    procedure Save1Click(Sender: TObject);
    procedure SaveAs1Click(Sender: TObject);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ZoomIn1Click(Sender: TObject);
    procedure ZoomOut1Click(Sender: TObject);
    procedure GridButton1Click(Sender: TObject);
    procedure BackgroundPalette1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ForegroundPalette1MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FreeDrawSpeedButtonClick(Sender: TObject);
    procedure EraseTextSpeedButtonClick(Sender: TObject);
    procedure FloodFillSpeedButtonClick(Sender: TObject);
    procedure RectSpeedButtonClick(Sender: TObject);
    procedure FillRectSpeedButtonClick(Sender: TObject);
    procedure LineSpeedButtonClick(Sender: TObject);
    procedure TextSpeedButtonClick(Sender: TObject);
    procedure TextColorChangeSpeedButtonClick(Sender: TObject);
    procedure SpecialCharSpeedButton1Click(Sender: TObject);
    procedure SpecialCharSpeedButton2Click(Sender: TObject);
    procedure CharRectSpeedButtonClick(Sender: TObject);
    procedure BlinkOnSpeedButtonClick(Sender: TObject);
    procedure BlinkOffSpeedButtonClick(Sender: TObject);
    procedure CursorBlinkTimerTimer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure EclipseSpeedButtonClick(Sender: TObject);
    procedure FilledEclipseSpeedButtonClick(Sender: TObject);
    procedure SpecialCharSelectSpeedButton1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpecialCharSelectSpeedButton1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SpecialCharSelectSpeedButton1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SpecialCharSelectSpeedButton2MouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SpecialCharSelectSpeedButton2MouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure SpecialCharSelectSpeedButton2MouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Cut1Click(Sender: TObject);
    procedure PaintBoxPopupMenu1Popup(Sender: TObject);
    procedure Export1Click(Sender: TObject);
    procedure Onlinedocumentation1Click(Sender: TObject);
  private
    { Private declarations }
    buffer, exportbuffer: TBitmap;
    drawbuffer: TBitmap;
    changed: boolean;
    needsupdate: boolean;
    undoManager: TUndoRedoManager;
    filemenuhistory: TFileMenuHistory;
    ffilename: string;
    escreen, backscreen: TEndScreen;
    zoom: integer;
    flastzoomwheel: int64;
    blink: boolean;
    lmousedown: boolean;
    lmouserecalcdown: boolean;
    lmousetraceposition: boolean;
    lmouseclearonmove: boolean;
    lmousedownx, lmousedowny: integer;
    lmousemovex, lmousemovey: integer;
    lcursorx, lcursory: integer;
    lcursortic: LongWord;
    bkcolor, fgcolor: LongWord;
    bkpalbitmap, fgpalbitmap: TBitmap;
    specialcharbitmap: TBitmap;
    specialchar1: Char;
    specialchar2: Char;
    specialcharLastClick: TPoint;
    closing: boolean;
    textartmethod: textartmethod_t;
    procedure Idle(Sender: TObject; var Done: Boolean);
    procedure Hint(Sender: TObject);
    procedure UpdateEnable;
    procedure InvalidatePaintBox;
    procedure DrawGrid;
    procedure DrawCursor;
    function ZoomValueX(const X: Integer): Integer;
    function ZoomValueY(const Y: Integer): Integer;
    procedure LLeftMousePaintAt(const X, Y: integer);
    procedure LLeftMousePaintTo(const X, Y: integer);
    procedure CreateDrawBuffer;
    procedure CreateExportBuffer;
    procedure DoPrepareEditor;
    procedure DoCreateNew;
    procedure DoLoadFromStream(const s: TStream);
    procedure DoSaveToStream(const s: TStream);
    procedure DoLoadUndo(const s: TStream);
    procedure DoSaveUndo(const s: TStream);
    function DoLoadFromFile(const aname: string): boolean;
    procedure FileNotFoundMsg(const aname: string);
    procedure DoSaveToFile(const aname: string);
    procedure OnLoadFileMenuHistory(Sender: TObject; const aname: string);
    function CheckCanClose: boolean;
    procedure SetFileName(const fname: string);
    procedure HandlePaletteImage(const X, Y: integer; const Palette1: TImage;
      const palbitmap: TBitmap; const tx: string; var cc: LongWord);
    procedure HandleCharactersImage(const X, Y: integer; const Characters1: TImage;
      const SpecialCharSpeedButton: TSpeedButton; const charbitmap: TBitmap;
      const makebk: boolean; var ch: Char);
    function lcursordown: boolean;
    procedure EditActionFreeDrawBackground(const X, Y: integer);
    procedure EditActionEraseText(const X, Y: integer);
    procedure EditActionFloodFill(const X, Y: integer);
    procedure EditActionRect(const X, Y: integer);
    procedure EditActionFillRect(const X, Y: integer);
    procedure EditActionLine(const X, Y: integer);
    procedure EditActionText(const X, Y: integer);
    procedure midpointellipse(const X, Y: integer; const filled: boolean);
    procedure EditActionEclipse(const X, Y: integer);
    procedure EditActionFillEclipse(const X, Y: integer);
    procedure EditActionSpecialCharacter1(const X, Y: integer);
    procedure EditActionSpecialCharacter2(const X, Y: integer);
    procedure EditActionTextColorChange(const X, Y: integer);
    procedure EditActionCharRectChange(const X, Y: integer;
      const atopleft, atopright, abottomleft, abottomright, ahorz, avert: integer);
    procedure EditActionBlink(const X, Y: integer; const ablink: Boolean);
    procedure DoExportImage(const imgfname: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  ee_utils, ee_defs, frm_imgconvertmethod;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: integer;
  doCreate: boolean;
begin
  DoubleBuffered := True;
  for i := 0 to ComponentCount - 1 do
    if Components[i].InheritsFrom(TWinControl) then
      if not (Components[i] is TListBox) then
        (Components[i] as TWinControl).DoubleBuffered := True;

  closing := False;

  lmousedown := False;
  lmouserecalcdown := True;
  lmousetraceposition := True;
  lmouseclearonmove := False;
  lmousedownx := 0;
  lmousedowny := 0;
  lmousemovex := 0;
  lmousemovey := 0;

  lcursorx := 0;
  lcursory := 0;
  lcursortic := 0;

  buffer := TBitmap.Create;
  buffer.Width := 640;
  buffer.Height := 200;
  buffer.PixelFormat := pf32bit;

  exportbuffer := TBitmap.Create;
  exportbuffer.Width := 640;
  exportbuffer.Height := 400;
  exportbuffer.PixelFormat := pf32bit;

  drawbuffer := TBitmap.Create;
  drawbuffer.Width := 640;
  drawbuffer.Height := 400;
  drawbuffer.PixelFormat := pf32bit;

  flastzoomwheel := GetTickCount;

  escreen := TEndScreen.Create;
  backscreen := TEndScreen.Create;

  blink := False;

  undoManager := TUndoRedoManager.Create;
  undoManager.OnLoadFromStream := DoLoadUndo;
  undoManager.OnSaveToStream := DoSaveUndo;

  ee_LoadSettingFromFile(ChangeFileExt(ParamStr(0), '.ini'));

  filemenuhistory := TFileMenuHistory.Create(self);
  filemenuhistory.MenuItem0 := HistoryItem0;
  filemenuhistory.MenuItem1 := HistoryItem1;
  filemenuhistory.MenuItem2 := HistoryItem2;
  filemenuhistory.MenuItem3 := HistoryItem3;
  filemenuhistory.MenuItem4 := HistoryItem4;
  filemenuhistory.MenuItem5 := HistoryItem5;
  filemenuhistory.MenuItem6 := HistoryItem6;
  filemenuhistory.MenuItem7 := HistoryItem7;
  filemenuhistory.MenuItem8 := HistoryItem8;
  filemenuhistory.MenuItem9 := HistoryItem9;
  filemenuhistory.OnOpen := OnLoadFileMenuHistory;

  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory9));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory8));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory7));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory6));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory5));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory4));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory3));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory2));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory1));
  filemenuhistory.AddPath(bigstringtostring(@opt_filemenuhistory0));

  ffilename := '';

  GridButton1.Down := opt_showgrid;
  zoom := GetIntInRange(opt_zoom, MINZOOM, MAXZOOM);

  textartmethod := textartmethod_t(GetIntInRange(opt_textartmethod, 0, Ord(NUMTEXTARTMETHOD) - 1));

  bkpalbitmap := TBitmap.Create;
  bkpalbitmap.Assign(BackgroundPalette1.Picture.Bitmap);
  HandlePaletteImage(BackgroundPalette1.Width - 1, BackgroundPalette1.Height - 1, BackgroundPalette1, bkpalbitmap, 'BK', bkcolor);

  fgpalbitmap := TBitmap.Create;
  fgpalbitmap.Assign(ForegroundPalette1.Picture.Bitmap);
  HandlePaletteImage(ForegroundPalette1.Width - 1, ForegroundPalette1.Height - 1, ForegroundPalette1, fgpalbitmap, 'FG', fgcolor);

  specialcharbitmap := TBitmap.Create;
  specialcharbitmap.Assign(SpecialCharacters1.Picture.Bitmap);
  HandleCharactersImage(1, 1, SpecialCharacters1, SpecialCharSpeedButton1, specialcharbitmap, True, specialchar1);
  HandleCharactersImage(1, 1, SpecialCharacters1, SpecialCharSpeedButton2, specialcharbitmap, False, specialchar2);

  doCreate := True;
  if ParamCount > 0 then
    if DoLoadFromFile(ParamStr(1)) then
      doCreate := False;

  if docreate then
  begin
    needsupdate := True;
    DoCreateNew;
  end;

  Application.OnIdle := Idle;
  Application.OnHint := Hint;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, drawbuffer);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  closing := True;

  undoManager.Free;

  stringtobigstring(filemenuhistory.PathStringIdx(0), @opt_filemenuhistory0);
  stringtobigstring(filemenuhistory.PathStringIdx(1), @opt_filemenuhistory1);
  stringtobigstring(filemenuhistory.PathStringIdx(2), @opt_filemenuhistory2);
  stringtobigstring(filemenuhistory.PathStringIdx(3), @opt_filemenuhistory3);
  stringtobigstring(filemenuhistory.PathStringIdx(4), @opt_filemenuhistory4);
  stringtobigstring(filemenuhistory.PathStringIdx(5), @opt_filemenuhistory5);
  stringtobigstring(filemenuhistory.PathStringIdx(6), @opt_filemenuhistory6);
  stringtobigstring(filemenuhistory.PathStringIdx(7), @opt_filemenuhistory7);
  stringtobigstring(filemenuhistory.PathStringIdx(8), @opt_filemenuhistory8);
  stringtobigstring(filemenuhistory.PathStringIdx(9), @opt_filemenuhistory9);
  opt_showgrid := GridButton1.Down;
  opt_zoom := zoom;
  opt_textartmethod := Ord(textartmethod);

  ee_SaveSettingsToFile(ChangeFileExt(ParamStr(0), '.ini'));

  filemenuhistory.Free;

  buffer.Free;
  exportbuffer.Free;
  drawbuffer.Free;

  bkpalbitmap.Free;
  fgpalbitmap.Free;
  specialcharbitmap.Free;

  escreen.Free;
  backscreen.Free;
end;

procedure TForm1.Idle(Sender: TObject; var Done: Boolean);
begin
  UpdateEnable;
end;

procedure TForm1.UpdateEnable;
begin
  Undo1.Enabled := undoManager.CanUndo;
  Redo1.Enabled := undoManager.CanRedo;
  UndoSpeedButton1.Enabled := undoManager.CanUndo;
  RedoSpeedButton1.Enabled := undoManager.CanRedo;
  Paste1.Enabled := Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_TEXT);
  PasteSpeedButton1.Enabled := Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_TEXT);
  ZoomInSpeedButton1.Enabled := zoom < MAXZOOM;
  ZoomOutSpeedButton1.Enabled := zoom > MINZOOM;
  if needsupdate then
  begin
    InvalidatePaintBox;
    needsupdate := False;
  end;
end;

procedure TForm1.Paste1Click(Sender: TObject);
var
  bm: TBitmap;
  lst: TStringList;
  s: string;
  tx, ty: integer;
  i, j: integer;
begin
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    bm := TBitmap.Create;
    try
      bm.LoadFromClipboardFormat(CF_BITMAP, ClipBoard.GetAsHandle(cf_Bitmap), 0);
      if ChooseImgConvertMethod(textartmethod) then
      begin
        Screen.Cursor := crHourglass;
        undoManager.SaveUndo;
        Changed := True;
        try
          BitmapToScreen(bm, escreen, textartmethod);
        finally
          Screen.Cursor := crDefault;
        end;
      end;
    finally
      bm.Free;
    end;
    InvalidatePaintBox;
  end
  else if Clipboard.HasFormat(CF_TEXT) then
  begin
    lst := TStringList.Create;
    try
      lst.Text := ClipBoard.AsText;
      if lcursordown then
      begin
        tx := lcursorx;
        ty := lcursory;
      end
      else
      begin
        tx := 0;
        ty := 0;
      end;
      undoManager.SaveUndo;
      Changed := True;
      for i := 0 to lst.Count - 1 do
      begin
        if ty >= SCREENSIZEY then
          Break;
        s := lst.Strings[i];
        if Length(s) > SCREENSIZEX then
          SetLength(s, SCREENSIZEX);
        for j := 1 to Length(s) do
        begin
          escreen.ForegroundColor[tx, ty] := fgcolor;
          escreen.Character[tx, ty] := s[j];
          inc(tx);
          if tx >= SCREENSIZEX then
            Break;
        end;
        if i < lst.Count - 1 then
        begin
          tx := 0;
          inc(ty);
        end;
      end;
      lcursorx := GetIntInRange(tx, 0, SCREENSIZEX - 1);
      lcursory := GetIntInRange(ty, 0, SCREENSIZEY - 1);
    finally
      lst.Free;
    end;
  end;
end;

procedure TForm1.Hint(Sender: TObject);
begin
  StatusBar1.SimpleText := Application.Hint;
end;

resourcestring
  rsTitle = 'ENDTEXT Editor';

procedure TForm1.About1Click(Sender: TObject);
begin
  MessageBox(
    Handle,
    PChar(Format('%s'#13#10'Version %s'#13#10#13#10'A tool for creating ENDTEXT screens.'#13#10'� 2021, jvalavanis@gmail.com', [rsTitle, I_VersionBuilt])),
    PChar(rsTitle),
    MB_OK or MB_ICONINFORMATION or MB_APPLMODAL);
end;

procedure TForm1.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.Copy1Click(Sender: TObject);
begin
  CreateExportBuffer;
  Clipboard.Assign(exportbuffer);
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  if OpenDialog1.Execute then
    if not DoLoadFromFile(OpenDialog1.FileName) then
      FileNotFoundMsg(OpenDialog1.FileName);
end;

procedure TForm1.InvalidatePaintBox;
begin
  CreateDrawBuffer;
  PaintBox1.Width := drawbuffer.Width;
  PaintBox1.Height := drawbuffer.Height;
  if TextSpeedButton.Down then
    PaintBox1.Cursor := crIBeam
  else
    PaintBox1.Cursor := crCross;
  PaintBox1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  blink := not blink;
  UpdateEnable;
  InvalidatePaintBox;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  Paste1.Enabled := Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_TEXT);
  Undo1.Enabled := undoManager.CanUndo;
  Redo1.Enabled := undoManager.CanRedo;
end;

function TForm1.lcursordown: boolean;
begin
  Result := TextSpeedButton.Down;
end;

procedure TForm1.EditActionFreeDrawBackground(const X, Y: integer);
begin
  escreen.BackgroundColor[X, Y] := bkcolor;
end;

procedure TForm1.EditActionEraseText(const X, Y: integer);
begin
  escreen.Character[X, Y] := #0;
end;

procedure TForm1.EditActionFloodFill(const X, Y: integer);
var
  rover: LongWord;
begin
  rover := escreen.BackgroundColor[X, Y];
  if rover <> bkcolor then
  begin
    escreen.BackgroundColor[X, Y] := bkcolor;
    if X > 0 then
      if escreen.BackgroundColor[X - 1, Y] = rover then
        EditActionFloodFill(X - 1, Y);
    if X < SCREENSIZEX - 1 then
      if escreen.BackgroundColor[X + 1, Y] = rover then
        EditActionFloodFill(X + 1, Y);
    if Y > 0 then
      if escreen.BackgroundColor[X, Y - 1] = rover then
        EditActionFloodFill(X, Y - 1);
    if Y < SCREENSIZEY - 1 then
      if escreen.BackgroundColor[X, Y + 1] = rover then
        EditActionFloodFill(X, Y + 1);
  end;
end;

procedure TForm1.EditActionRect(const X, Y: integer);
var
  atop, abottom, aleft, aright: integer;
  i: integer;
begin
  aleft := MinI(lmousedownx, X);
  aright := MaxI(lmousedownx, X);
  atop := MinI(lmousedowny, Y);
  abottom := MaxI(lmousedowny, Y);
  for i := aleft to aright do
  begin
    escreen.BackgroundColor[i, atop] := bkcolor;
    escreen.BackgroundColor[i, abottom] := bkcolor;
  end;
  for i := atop + 1 to abottom - 1 do
  begin
    escreen.BackgroundColor[aleft, i] := bkcolor;
    escreen.BackgroundColor[aright, i] := bkcolor;
  end;
end;

procedure TForm1.EditActionFillRect(const X, Y: integer);
var
  atop, abottom, aleft, aright: integer;
  i, j: integer;
begin
  aleft := MinI(lmousedownx, X);
  aright := MaxI(lmousedownx, X);
  atop := MinI(lmousedowny, Y);
  abottom := MaxI(lmousedowny, Y);
  for i := aleft to aright do
    for j := atop to abottom do
      escreen.BackgroundColor[i, j] := bkcolor;
end;

procedure TForm1.EditActionLine(const X, Y: integer);
begin
  escreen.BackgroundColor[X, Y] := bkcolor;
end;

procedure TForm1.EditActionText(const X, Y: integer);
begin
  lcursorx := X;
  lcursory := Y;
end;

procedure TForm1.midpointellipse(const X, Y: integer; const filled: boolean);
// midpoint ellipse algorithm
var
  rx, ry, xc, yc: integer;
  dx, dy, d1, d2: single;
  xx, yy: integer;
  i: integer;

  procedure _draw_point(const ax, ay: integer);
  begin
    if IsIntInRange(ax, 0, SCREENSIZEX - 1) then
      if IsIntInRange(ay, 0, SCREENSIZEY - 1) then
        escreen.BackgroundColor[ax, ay] := bkcolor;
  end;

begin
  rx := abs(lmousedownx - X);
  ry := abs(lmousedowny - Y);
  xc := lmousedownx;
  yc := lmousedowny;
  xx := 0;
  yy := ry;

  // Initial decision parameter of region 1
  d1 := (ry * ry) -
        (rx * rx * ry) +
        (0.25 * rx * rx);
  dx := 2 * ry * ry * xx;
  dy := 2 * rx * rx * yy;

  // For region 1
  while dx < dy do
  begin
    // Print points based on 4-way symmetry
    _draw_point(xx + xc, yy + yc);
    _draw_point(-xx + xc, yy + yc);
    _draw_point(xx + xc, -yy + yc);
    _draw_point(-xx + xc, -yy + yc);

    if filled then
      for i := -xx + xc + 1 to xx + xc - 1 do
      begin
        _draw_point(i, yy + yc);
        _draw_point(i, -yy + yc);
      end;

    // Checking and updating value of
    // decision parameter based on algorithm
    if d1 < 0 then
    begin
      inc(xx);
      dx := dx + (2 * ry * ry);
      d1 := d1 + dx + (ry * ry);
    end
    else
    begin
      inc(xx);
      dec(yy);
      dx := dx + (2 * ry * ry);
      dy := dy - (2 * rx * rx);
      d1 := d1 + dx - dy + (ry * ry);
    end;
  end;

  // Decision parameter of region 2
  d2 := ((ry * ry) * ((xx + 0.5) * (xx + 0.5))) +
        ((rx * rx) * ((yy - 1) * (yy - 1))) -
        (rx * rx * ry * ry);

  // Plotting points of region 2
  while yy >= 0 do
  begin
    // printing points based on 4-way symmetry
    _draw_point(xx + xc, yy + yc);
    _draw_point(-xx + xc, yy + yc);
    _draw_point(xx + xc, -yy + yc);
    _draw_point(-xx + xc, -yy + yc);

    if filled then
      for i := -xx + xc + 1 to xx + xc - 1 do
      begin
        _draw_point(i, yy + yc);
        _draw_point(i, -yy + yc);
      end;

    // Checking and updating parameter
    // value based on algorithm
    if d2 > 0 then
    begin
      dec(yy);
      dy := dy - (2 * rx * rx);
      d2 := d2 + (rx * rx) - dy;
    end
    else
    begin
      dec(yy);
      inc(xx);
      dx := dx + (2 * ry * ry);
      dy := dy - (2 * rx * rx);
      d2 := d2 + dx - dy + (rx * rx);
    end;
  end;
end;

procedure TForm1.EditActionEclipse(const X, Y: integer);
begin
  midpointellipse(X, Y, False);
end;

procedure TForm1.EditActionFillEclipse(const X, Y: integer);
begin
  midpointellipse(X, Y, True);
end;

procedure TForm1.EditActionSpecialCharacter1(const X, Y: integer);
begin
  escreen.BackgroundColor[X, Y] := bkcolor;
  escreen.ForegroundColor[X, Y] := fgcolor;
  escreen.Character[X, Y] := specialchar1;
end;

procedure TForm1.EditActionSpecialCharacter2(const X, Y: integer);
begin
  escreen.ForegroundColor[X, Y] := fgcolor;
  escreen.Character[X, Y] := specialchar2;
end;

procedure TForm1.EditActionTextColorChange(const X, Y: integer);
begin
  escreen.ForegroundColor[X, Y] := fgcolor;
end;

procedure TForm1.EditActionCharRectChange(const X, Y: integer;
  const atopleft, atopright, abottomleft, abottomright, ahorz, avert: integer);
var
  atop, abottom, aleft, aright: integer;
  i: integer;
begin
  aleft := MinI(lmousedownx, X);
  aright := MaxI(lmousedownx, X);
  atop := MinI(lmousedowny, Y);
  abottom := MaxI(lmousedowny, Y);
  for i := aleft + 1 to aright - 1 do
  begin
    escreen.ForegroundColor[i, atop] := fgcolor;
    escreen.Character[i, atop] := Chr(ahorz);
    escreen.ForegroundColor[i, abottom] := fgcolor;
    escreen.Character[i, abottom] := Chr(ahorz);
  end;
  for i := atop + 1 to abottom - 1 do
  begin
    escreen.ForegroundColor[aleft, i] := fgcolor;
    escreen.Character[aleft, i] := Chr(avert);
    escreen.ForegroundColor[aright, i] := fgcolor;
    escreen.Character[aright, i] := Chr(avert);
  end;
  escreen.ForegroundColor[aleft, atop] := fgcolor;
  escreen.Character[aleft, atop] := Chr(atopleft);
  escreen.ForegroundColor[aright, atop] := fgcolor;
  escreen.Character[aright, atop] := Chr(atopright);
  escreen.ForegroundColor[aleft, abottom] := fgcolor;
  escreen.Character[aleft, abottom] := Chr(abottomleft);
  escreen.ForegroundColor[aright, abottom] := fgcolor;
  escreen.Character[aright, abottom] := Chr(abottomright);
end;

procedure TForm1.EditActionBlink(const X, Y: integer; const ablink: Boolean);
begin
  escreen.Blink[X, Y] := ablink;
end;

procedure TForm1.LLeftMousePaintAt(const X, Y: integer);
begin
  if not lmousedown then
    Exit;
  if FreeDrawSpeedButton.Down then
    EditActionFreeDrawBackground(X, Y)
  else if FloodFillSpeedButton.Down then
    EditActionFloodFill(X, Y)
  else if EraseTextSpeedButton.Down then
    EditActionEraseText(X, Y)
  else if RectSpeedButton.Down then
    EditActionRect(X, Y)
  else if FillRectSpeedButton.Down then
    EditActionFillRect(X, Y)
  else if LineSpeedButton.Down then
    EditActionLine(X, Y)
  else if lcursordown then
    EditActionText(X, Y)
  else if EclipseSpeedButton.Down then
    EditActionEclipse(X, Y)
  else if FilledEclipseSpeedButton.Down then
    EditActionFillEclipse(X, Y)
  else if SpecialCharSpeedButton1.Down then
    EditActionSpecialCharacter1(X, Y)
  else if SpecialCharSpeedButton2.Down then
    EditActionSpecialCharacter2(X, Y)
  else if TextColorChangeSpeedButton.Down then
    EditActionTextColorChange(X, Y)
  else if CharRect1SpeedButton.Down then
    EditActionCharRectChange(X, Y, $DA, $BF, $C0, $D9, $C4, $B3)
  else if CharRect2SpeedButton.Down then
    EditActionCharRectChange(X, Y, $D5, $B8, $D4, $BE, $CD, $B3)
  else if CharRect3SpeedButton.Down then
    EditActionCharRectChange(X, Y, $D6, $B7, $D3, $BD, $C4, $BA)
  else if CharRect4SpeedButton.Down then
    EditActionCharRectChange(X, Y, $C9, $BB, $C8, $BC, $CD, $BA)
  else if CharRect5SpeedButton.Down then
    EditActionCharRectChange(X, Y, $DB, $DB, $DB, $DB, $DB, $DB)
  else if BlinkOnSpeedButton.Down then
    EditActionBlink(X, Y, True)
  else if BlinkOffSpeedButton.Down then
    EditActionBlink(X, Y, False);
end;

procedure TForm1.LLeftMousePaintTo(const X, Y: integer);
var
  dx, dy: integer;
  curx, cury: integer;
  sx, sy,
  ax, ay,
  d: integer;
begin
  if not lmousedown then
    Exit;

  if lmouseclearonmove then
    escreen.AssignTo(backscreen);

  if lmousetraceposition then
  begin
    dx := X - lmousedownx;
    ax := 2 * abs(dx);
    if dx < 0 then
      sx := -1
    else
      sx := 1;
    dy := Y - lmousedowny;
    ay := 2 * abs(dy);
    if dy < 0 then
      sy := -1
    else
      sy := 1;

    curx := lmousedownx;
    cury := lmousedowny;

    if ax > ay then
    begin
      d := ay - ax div 2;
      while True do
      begin
        LLeftMousePaintAt(curx, cury);
        if curx = X then break;
        if d >= 0 then
        begin
          cury := cury + sy;
          d := d - ax;
        end;
        curx := curx + sx;
        d := d + ay;
      end;
    end
    else
    begin
      d := ax - ay div 2;
      while True do
      begin
        LLeftMousePaintAt(curx, cury);
        if cury = Y then break;
        if d >= 0 then
        begin
          curx := curx + sx;
          d := d - ay;
        end;
        cury := cury + sy;
        d := d + ax;
      end;
    end;
  end
  else
  begin
    LLeftMousePaintAt(X, Y);
  end;
  if not lcursordown then
    Changed := True;
  InvalidatePaintBox;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then
  begin
    if not lcursordown then
      undoManager.SaveUndo;

    backscreen.AssignTo(escreen);

    lmousedown := True;
    lmousedownx := ZoomValueX(X);
    lmousedowny := ZoomValueY(Y);
    lmousemovex := ZoomValueX(X);
    lmousemovey := ZoomValueY(Y);

    LLeftMousePaintTo(lmousemovex, lmousemovey);
  end;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then
  begin
    lmousemovex := ZoomValueX(X);
    lmousemovey := ZoomValueY(Y);
    LLeftMousePaintTo(lmousemovex, lmousemovey);
    lmousedown := False;
  end;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  lmousemovex := ZoomValueX(X);
  lmousemovey := ZoomValueY(Y);
  StatusBar1.SimpleText := Format(
    'Position: (%d, %d), Background: (#%s), Foreground: (#%s), Text: (#%s)',
      [lmousemovex, lmousemovey,
       IntToHex(escreen.BackgroundColor[lmousemovex, lmousemovey], 6),
       IntToHex(escreen.ForegroundColor[lmousemovex, lmousemovey], 6),
       IntToHex(Ord(escreen.Character[lmousemovex, lmousemovey]), 2)]);
  if lmousedown then
  begin
    LLeftMousePaintTo(lmousemovex, lmousemovey);
    if lmouserecalcdown then
    begin
      lmousedownx := ZoomValueX(X);
      lmousedowny := ZoomValueY(Y);
    end;
  end
end;

procedure TForm1.DrawGrid;
var
  x, y: integer;
  stepw, steph: integer;
begin
  if GridButton1.Down then
  begin
    drawbuffer.Canvas.Pen.Style := psSolid;
    drawbuffer.Canvas.Pen.Color := RGB(160, 160, 128);
    stepw := drawbuffer.Width div SCREENSIZEX;

    for x := 1 to SCREENSIZEX - 1 do
    begin
      drawbuffer.Canvas.MoveTo(x * stepw - 1, 0);
      drawbuffer.Canvas.LineTo(x * stepw - 1, drawbuffer.Height);
    end;

    steph := drawbuffer.Height div SCREENSIZEY;
    for y := 1 to SCREENSIZEY - 1 do
    begin
      drawbuffer.Canvas.MoveTo(0, y * steph - 1);
      drawbuffer.Canvas.LineTo(drawbuffer.Width, y * steph - 1);
    end;
  end;

end;

procedure TForm1.DrawCursor;
var
  stepw, steph: integer;
begin
  if lcursordown then
  begin
    drawbuffer.Canvas.Pen.Style := psSolid;
    if Odd(lcursortic) then
      drawbuffer.Canvas.Pen.Color := RGB(255, 255, 255)
    else
      drawbuffer.Canvas.Pen.Color := RGB(0, 0, 0);

    stepw := drawbuffer.Width div SCREENSIZEX;
    steph := drawbuffer.Height div SCREENSIZEY;

    drawbuffer.Canvas.MoveTo(lcursorx * stepw, lcursory * steph);
    drawbuffer.Canvas.LineTo(lcursorx * stepw, (lcursory + 1) * steph - 2);
    drawbuffer.Canvas.LineTo((lcursorx + 1) * stepw - 2, (lcursory + 1) * steph - 2);
    drawbuffer.Canvas.LineTo((lcursorx + 1) * stepw - 2, lcursory * steph);
    drawbuffer.Canvas.LineTo(lcursorx * stepw, lcursory * steph);
  end;
end;

function TForm1.ZoomValueX(const X: Integer): Integer;
begin
  Result := GetIntInRange(X div (drawbuffer.Width div SCREENSIZEX), 0, SCREENSIZEX - 1);
end;

function TForm1.ZoomValueY(const Y: Integer): Integer;
begin
  Result := GetIntInRange(Y div (drawbuffer.Height div SCREENSIZEY), 0, SCREENSIZEY - 1);
end;

procedure TForm1.CreateDrawBuffer;
begin
  escreen.GetBitmap(buffer, blink);
  drawbuffer.Width := 640 + SCREENSIZEX * zoom;
  drawbuffer.Height := 400 + (2 * SCREENSIZEY) * zoom;

  drawbuffer.Canvas.StretchDraw(Rect(0, 0, drawbuffer.Width, drawbuffer.Height), buffer);
  DrawGrid;
  DrawCursor;
end;

procedure TForm1.CreateExportBuffer;
begin
  escreen.GetBitmap(buffer, blink);
  exportbuffer.Canvas.StretchDraw(Rect(0, 0, exportbuffer.Width, exportbuffer.Height), buffer);
end;

procedure TForm1.Undo1Click(Sender: TObject);
begin
  if undoManager.CanUndo then
    undoManager.Undo;
end;

procedure TForm1.Redo1Click(Sender: TObject);
begin
  if undoManager.CanRedo then
    undoManager.Redo;
end;

procedure TForm1.File1Click(Sender: TObject);
begin
  filemenuhistory.RefreshMenuItems;
end;

procedure TForm1.DoPrepareEditor;
begin
  lcursorx := 0;
  lcursory := 0;
  lcursortic := 0;
  undoManager.Clear;
  blink := False;
  changed := False;
end;

procedure TForm1.DoCreateNew;
begin
  escreen.Clear;
  DoPrepareEditor;
  SetFileName('');
end;

procedure TForm1.DoLoadFromStream(const s: TStream);
begin
  needsupdate := True;
  escreen.LoadFromStream(s);
end;

procedure TForm1.DoSaveToStream(const s: TStream);
begin
  escreen.SaveToStream(s);
end;

procedure TForm1.DoLoadUndo(const s: TStream);
begin
  DoLoadFromStream(s);
  changed := True;
end;

procedure TForm1.DoSaveUndo(const s: TStream);
begin
  DoSaveToStream(s);
end;

function TForm1.DoLoadFromFile(const aname: string): boolean;
var
  fs: TFileStream;
begin
  if not FileExists(aname) then
  begin
    Result := False;
    Exit;
  end;

  Result := True;
  fs := TFileStream.Create(aname, fmOpenRead);
  try
    DoLoadFromStream(fs);
    DoPrepareEditor;
    SetFileName(aname);
    filemenuhistory.AddPath(aname);
  finally
    fs.Free;
  end;
end;

procedure TForm1.FileNotFoundMsg(const aname: string);
begin
  ShowMessage(Format('Can not find file %s', [mkshortname(aname)]));
end;

procedure TForm1.DoSaveToFile(const aname: string);
var
  fs: TFileStream;
begin
  BackupFile(aname);
  fs := TFileStream.Create(aname, fmCreate);
  try
    DoSaveToStream(fs);
    SetFileName(aname);
    filemenuhistory.AddPath(aname);
    changed := False;
  finally
    fs.Free;
  end;
end;

procedure TForm1.OnLoadFileMenuHistory(Sender: TObject; const aname: string);
begin
  if CheckCanClose then
    if not DoLoadFromFile(aname) then
      FileNotFoundMsg(aname);
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := CheckCanClose;
end;

function TForm1.CheckCanClose: boolean;
var
  ret: integer;
begin
  if changed then
  begin
    ret := MessageBox(Handle, 'Do you want to save changes?', PChar(rsTitle), MB_YESNOCANCEL or MB_ICONQUESTION or MB_APPLMODAL);
    if ret = IDCANCEL	then
    begin
      Result := False;
      exit;
    end;
    if ret = IDNO	then
    begin
      Result := True;
      exit;
    end;
    if ret = IDYES then
    begin
      Save1Click(self);
      Result := not changed;
      exit;
    end;
  end;
  Result := True;
end;

procedure TForm1.New1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  DoCreateNew;
end;

procedure TForm1.SetFileName(const fname: string);
begin
  ffilename := fname;
  Caption := rsTitle;
  if ffilename <> '' then
    Caption := Caption + ' - ' + MkShortName(ffilename);
end;

procedure TForm1.Save1Click(Sender: TObject);
begin
  if ffilename = '' then
  begin
    SaveAs1Click(Sender);
    Exit;
  end;

  DoSaveToFile(ffilename);
end;

procedure TForm1.SaveAs1Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    DoSaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
  tick: int64;
begin
  tick := GetTickCount;
  if tick <= flastzoomwheel + MOUSEWHEELTIMEOUT then
    Exit;
  flastzoomwheel := tick;
  pt := PaintBox1.Parent.ScreenToClient(MousePos);
  r := PaintBox1.ClientRect;
  if r.Right > ScrollBox1.Width then
    r.Right := ScrollBox1.Width;
  if r.Bottom > ScrollBox1.Height then
    r.Bottom := ScrollBox1.Height;
  if PtInRect(r, pt) then
    ZoomOut1Click(Sender);
end;

procedure TForm1.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
var
  pt: TPoint;
  r: TRect;
  tick: int64;
begin
  tick := GetTickCount;
  if tick <= flastzoomwheel + MOUSEWHEELTIMEOUT then
    Exit;
  flastzoomwheel := tick;
  pt := PaintBox1.Parent.ScreenToClient(MousePos);
  r := PaintBox1.ClientRect;
  if r.Right > ScrollBox1.Width then
    r.Right := ScrollBox1.Width;
  if r.Bottom > ScrollBox1.Height then
    r.Bottom := ScrollBox1.Height;
  if PtInRect(r, pt) then
    ZoomIn1Click(Sender);
end;

procedure TForm1.ZoomIn1Click(Sender: TObject);
begin
  if zoom < MAXZOOM then
  begin
    inc(zoom);
    needsupdate := True;
  end;
end;

procedure TForm1.ZoomOut1Click(Sender: TObject);
begin
  if zoom > MINZOOM then
  begin
    dec(zoom);
    needsupdate := True;
  end;
end;

procedure TForm1.GridButton1Click(Sender: TObject);
begin
  InvalidatePaintBox;
end;

function constrastcolor(const cc: LongWord): LongWord;
var
  l: double;
begin
  l := GetRValue(cc) * 0.299 + GetGValue(cc) * 0.587 + GetBValue(cc) * 0.114;
  if l < 128 then
    Result := RGB(255, 255, 255)
  else
    Result := RGB(0, 0, 0);
end;

procedure TForm1.HandlePaletteImage(const X, Y: integer; const Palette1: TImage;
  const palbitmap: TBitmap; const tx: string; var cc: LongWord);
var
  px, py: integer;
  C: TCanvas;
  txcolor: LongWord;
begin
  if closing then
    Exit;

  if not IsIntInRange(X, 0, Palette1.Width - 1) then
    Exit;

  if not IsIntInRange(Y, 0, Palette1.Height - 1) then
    Exit;

  cc := palbitmap.Canvas.Pixels[X, Y];

  C := Palette1.Picture.Bitmap.Canvas;
  C.Draw(0, 0, palbitmap);
  px := X div COLORPICKPALETTESIZE;
  py := Y div COLORPICKPALETTESIZE;
  C.Pen.Style := psSolid;
  txcolor := constrastcolor(cc);
  C.Pen.Color := txcolor;
  C.MoveTo(px * COLORPICKPALETTESIZE, py * COLORPICKPALETTESIZE);
  C.LineTo((1 + px) * COLORPICKPALETTESIZE - 1, py * COLORPICKPALETTESIZE);
  C.LineTo((1 + px) * COLORPICKPALETTESIZE - 1, (1 + py) * COLORPICKPALETTESIZE - 1);
  C.LineTo(px * COLORPICKPALETTESIZE, (1 + py) * COLORPICKPALETTESIZE - 1);
  C.LineTo(px * COLORPICKPALETTESIZE, py * COLORPICKPALETTESIZE);
  C.Font.Size := 10;
  C.Font.Color := txcolor;
  C.Brush.Style := bsClear;
  C.TextOut(
    px * COLORPICKPALETTESIZE + COLORPICKPALETTESIZE div 2 - C.TextWidth(tx) div 2,
    py * COLORPICKPALETTESIZE + COLORPICKPALETTESIZE div 2 - C.TextHeight(tx) div 2,
    tx
  );
  Palette1.Invalidate;
end;

procedure TForm1.HandleCharactersImage(const X, Y: integer; const Characters1: TImage;
  const SpecialCharSpeedButton: TSpeedButton; const charbitmap: TBitmap;
  const makebk: boolean; var ch: Char);
var
  px, py: integer;
  C, C2: TCanvas;
  ix, iy, dx, dy: integer;
begin
  if closing then
    Exit;

  if not IsIntInRange(X, 0, Characters1.Width - 1) then
    Exit;

  if not IsIntInRange(Y, 0, Characters1.Height - 1) then
    Exit;

  specialcharLastClick.X := X;
  specialcharLastClick.Y := Y;

  C := Characters1.Picture.Bitmap.Canvas;
  C2 := SpecialCharSpeedButton.Glyph.Canvas;
  C.Draw(0, 0, charbitmap);
  px := X div CHARACTERPICKPALETTESIZE;
  py := Y div CHARACTERPICKPALETTESIZE;
  ch := Chr(py * (Characters1.Width div CHARACTERPICKPALETTESIZE) + px + 176);

  dx := px * CHARACTERPICKPALETTESIZE;
  dy := py * CHARACTERPICKPALETTESIZE;
  for ix := dx to (1 + px) * CHARACTERPICKPALETTESIZE - 1 do
    for iy := dy to (1 + py) * CHARACTERPICKPALETTESIZE - 1 do
      if C.Pixels[ix, iy] = RGB(0, 0, 0) then
      begin
        C.Pixels[ix, iy] := fgcolor;
        if makebk then
          C2.Pixels[ix - dx + 2, iy - dy + 2] := fgcolor
        else
          C2.Pixels[ix - dx + 2, iy - dy + 2] := RGB(0, 0, 0);
      end
      else
      begin
        C.Pixels[ix, iy] := bkcolor;
        if makebk then
          C2.Pixels[ix - dx + 2, iy - dy + 2] := bkcolor
        else
          C2.Pixels[ix - dx + 2, iy - dy + 2] := RGB(240, 240, 240);
      end;

  C.Pen.Style := psSolid;
  C.Pen.Color := RGB(255, 0, 0);
  C.MoveTo(px * CHARACTERPICKPALETTESIZE, py * CHARACTERPICKPALETTESIZE);
  C.LineTo((1 + px) * CHARACTERPICKPALETTESIZE - 1, py * CHARACTERPICKPALETTESIZE);
  C.LineTo((1 + px) * CHARACTERPICKPALETTESIZE - 1, (1 + py) * CHARACTERPICKPALETTESIZE - 1);
  C.LineTo(px * CHARACTERPICKPALETTESIZE, (1 + py) * CHARACTERPICKPALETTESIZE - 1);
  C.LineTo(px * CHARACTERPICKPALETTESIZE, py * CHARACTERPICKPALETTESIZE);
  Characters1.Invalidate;
end;

procedure TForm1.BackgroundPalette1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  HandlePaletteImage(X, Y, BackgroundPalette1, bkpalbitmap, 'BK', bkcolor);
  HandleCharactersImage(specialcharLastClick.X, specialcharLastClick.Y, SpecialCharacters1, SpecialCharSpeedButton1, specialcharbitmap, True, specialchar1);
  HandleCharactersImage(specialcharLastClick.X, specialcharLastClick.Y, SpecialCharacters1, SpecialCharSpeedButton2, specialcharbitmap, False, specialchar2);
end;

procedure TForm1.ForegroundPalette1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  HandlePaletteImage(X, Y, ForegroundPalette1, fgpalbitmap, 'FG', fgcolor);
  HandleCharactersImage(specialcharLastClick.X, specialcharLastClick.Y, SpecialCharacters1, SpecialCharSpeedButton1, specialcharbitmap, True, specialchar1);
  HandleCharactersImage(specialcharLastClick.X, specialcharLastClick.Y, SpecialCharacters1, SpecialCharSpeedButton2, specialcharbitmap, False, specialchar2);
end;

procedure TForm1.FreeDrawSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := True;
  lmousetraceposition := True;
  lmouseclearonmove := False;
end;

procedure TForm1.EraseTextSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := True;
  lmousetraceposition := True;
  lmouseclearonmove := False;
end;

procedure TForm1.FloodFillSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := True;
  lmousetraceposition := False;
  lmouseclearonmove := True;
end;

procedure TForm1.RectSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := False;
  lmousetraceposition := False;
  lmouseclearonmove := True;
end;

procedure TForm1.FillRectSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := False;
  lmousetraceposition := False;
  lmouseclearonmove := True;
end;

procedure TForm1.LineSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := False;
  lmousetraceposition := True;
  lmouseclearonmove := True;
end;

procedure TForm1.EclipseSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := False;
  lmousetraceposition := False;
  lmouseclearonmove := True;
end;


procedure TForm1.FilledEclipseSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := False;
  lmousetraceposition := False;
  lmouseclearonmove := True;
end;

procedure TForm1.TextSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := False;
  lmousetraceposition := False;
  lmouseclearonmove := False;
end;

procedure TForm1.TextColorChangeSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := True;
  lmousetraceposition := True;
  lmouseclearonmove := False;
end;

procedure TForm1.SpecialCharSpeedButton1Click(Sender: TObject);
begin
  lmouserecalcdown := True;
  lmousetraceposition := True;
  lmouseclearonmove := False;
end;

procedure TForm1.SpecialCharSpeedButton2Click(Sender: TObject);
begin
  lmouserecalcdown := True;
  lmousetraceposition := True;
  lmouseclearonmove := False;
end;

procedure TForm1.CharRectSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := False;
  lmousetraceposition := False;
  lmouseclearonmove := True;
end;

procedure TForm1.BlinkOnSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := True;
  lmousetraceposition := True;
  lmouseclearonmove := False;
end;

procedure TForm1.BlinkOffSpeedButtonClick(Sender: TObject);
begin
  lmouserecalcdown := True;
  lmousetraceposition := True;
  lmouseclearonmove := False;
end;

procedure TForm1.CursorBlinkTimerTimer(Sender: TObject);
begin
  if lcursordown then
  begin
    inc(lcursortic);
    needsupdate := True;
  end;
end;

procedure TForm1.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if not lcursordown then
    Exit;

  if key in [#32..#128] then
  begin
    escreen.ForegroundColor[lcursorx, lcursory] := fgcolor;
    if escreen.Character[lcursorx, lcursory] <> key then
    begin
      undoManager.SaveUndo;
      changed := True;
      escreen.Character[lcursorx, lcursory] := key;
      needsupdate := True;
    end;
    if lcursorx < SCREENSIZEX - 1 then
      inc(lcursorx)
    else if lcursory < SCREENSIZEY - 1 then
    begin
      lcursorx := 0;
      inc(lcursory);
    end;
  end
  else if key = #8 then
  begin
    if lcursorx > 0 then
      dec(lcursorx)
    else if lcursory > 0 then
    begin
      lcursorx := SCREENSIZEX - 1;
      dec(lcursory);
    end;
    if escreen.Character[lcursorx, lcursory] <> ' ' then
    begin
      undoManager.SaveUndo;
      changed := True;
      escreen.Character[lcursorx, lcursory] := ' ';
      needsupdate := True;
    end;
  end
  else if key = #13 then
  begin
    if lcursory < SCREENSIZEY - 1 then
    begin
      lcursorx := 0;
      inc(lcursory);
      needsupdate := True;
    end;
  end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if not lcursordown then
    Exit;

  if not (Shift = []) then
    Exit;

  case key of
    VK_DELETE:
      if escreen.Character[lcursorx, lcursory] <> ' ' then
      begin
        undoManager.SaveUndo;
        changed := True;
        escreen.Character[lcursorx, lcursory] := ' ';
        needsupdate := True;
      end;
    VK_END:
      begin
        lcursorx := SCREENSIZEX - 1;
        needsupdate := True;
      end;
    VK_HOME:
      begin
        lcursorx := 0;
        needsupdate := True;
      end;
    VK_LEFT:
      begin
        if lcursorx > 0 then
        begin
          dec(lcursorx);
          needsupdate := True;
        end;
      end;
    VK_RIGHT:
      begin
        if lcursorx < SCREENSIZEX - 1 then
        begin
          inc(lcursorx);
          needsupdate := True;
        end;
      end;
    VK_DOWN:
      begin
        if lcursory < SCREENSIZEY - 1 then
        begin
          inc(lcursory);
          needsupdate := True;
        end;
      end;
    VK_UP:
      begin
        if lcursory > 0 then
        begin
          dec(lcursory);
          needsupdate := True;
        end;
      end;
    33: // Page Up
      begin
        if lcursory > 4 then
          dec(lcursory, 5)
        else
          lcursory := 0;
        needsupdate := True;
      end;
    34: // Page Down
      begin
        if lcursory < SCREENSIZEY - 5 then
          inc(lcursory, 5)
        else
          lcursory := SCREENSIZEY - 1;
        needsupdate := True;
      end;
  end;
end;

procedure TForm1.SpecialCharSelectSpeedButton1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SpecialCharSpeedButton1.Down := True;
  SpecialCharSpeedButton1.OnClick(Sender);
  SpecialCharSelectSpeedButton1.Down := True;
  SpecialCharactersPanel1.Left := SpecialCharSpeedButton1.Left;
  SpecialCharactersPanel1.Top := SpecialCharSpeedButton1.Top + SpecialCharSpeedButton1.Height;
  SpecialCharactersPanel1.BringToFront;
  SpecialCharactersPanel1.Visible := True;
end;

procedure TForm1.SpecialCharSelectSpeedButton1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SpecialCharactersPanel1.Visible := False;
  SpecialCharSelectSpeedButton1.Down := False;
end;

procedure TForm1.SpecialCharSelectSpeedButton1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  p: TPoint;
begin
  if SpecialCharactersPanel1.Visible then
  begin
    p.X := X;
    p.Y := Y;
    p := SpecialCharSelectSpeedButton1.ClientToScreen(p);
    p := SpecialCharacters1.ScreenToClient(p);
    HandleCharactersImage(p.X, p.Y, SpecialCharacters1, SpecialCharSpeedButton1, specialcharbitmap, True, specialchar1);
  end
  else
    SpecialCharSelectSpeedButton1.Down := False;
end;

procedure TForm1.SpecialCharSelectSpeedButton2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SpecialCharSpeedButton2.Down := True;
  SpecialCharSpeedButton2.OnClick(Sender);
  SpecialCharSelectSpeedButton2.Down := True;
  SpecialCharactersPanel1.Left := SpecialCharSpeedButton2.Left;
  SpecialCharactersPanel1.Top := SpecialCharSpeedButton2.Top + SpecialCharSpeedButton2.Height;
  SpecialCharactersPanel1.BringToFront;
  SpecialCharactersPanel1.Visible := True;
end;

procedure TForm1.SpecialCharSelectSpeedButton2MouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  if SpecialCharactersPanel1.Visible then
  begin
    p.X := X;
    p.Y := Y;
    p := SpecialCharSelectSpeedButton2.ClientToScreen(p);
    p := SpecialCharacters1.ScreenToClient(p);
    HandleCharactersImage(p.X, p.Y, SpecialCharacters1, SpecialCharSpeedButton2, specialcharbitmap, False, specialchar2);
  end
  else
    SpecialCharSelectSpeedButton2.Down := False;
end;

procedure TForm1.SpecialCharSelectSpeedButton2MouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  SpecialCharactersPanel1.Visible := False;
  SpecialCharSelectSpeedButton2.Down := False;
end;

procedure TForm1.Cut1Click(Sender: TObject);
var
  x, y: integer;
begin
  Clipboard.Assign(buffer);
  undoManager.SaveUndo;
  Changed := True;
  for x := 0 to SCREENSIZEX - 1 do
    for y := 0 to SCREENSIZEY - 1 do
      escreen.BackgroundColor[x, y] := 0;
end;

procedure TForm1.PaintBoxPopupMenu1Popup(Sender: TObject);
begin
  Undo2.Enabled := undoManager.CanUndo;
  Redo2.Enabled := undoManager.CanRedo;
  Paste2.Enabled := Clipboard.HasFormat(CF_BITMAP) or Clipboard.HasFormat(CF_TEXT);
end;

procedure TForm1.Export1Click(Sender: TObject);
var
  imgfname: string;
begin
  if SavePictureDialog1.Execute then
  begin
    Screen.Cursor := crHourglass;
    try
      imgfname := SavePictureDialog1.FileName;
      BackupFile(imgfname);
      DoExportImage(imgfname);
    finally
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TForm1.DoExportImage(const imgfname: string);
var
  png: TPngObject;
  jpg: TJPEGImage;
  ext: string;
begin
  CreateExportBuffer;
  ext := UpperCase(ExtractFileExt(imgfname));
  if ext = '.PNG' then
  begin
    png := TPngObject.Create;
    png.Assign(exportbuffer);
    png.SaveToFile(imgfname);
    png.Free;
  end
  else if (ext = '.JPG') or (ext = '.JPEG') then
  begin
    jpg := TJPEGImage.Create;
    jpg.Assign(exportbuffer);
    jpg.SaveToFile(imgfname);
    jpg.Free;
  end
  else
    exportbuffer.SaveToFile(imgfname);
end;

procedure TForm1.Onlinedocumentation1Click(Sender: TObject);
begin
  I_GoToWebPage('https://endedit.sourceforge.io');
end;

end.

