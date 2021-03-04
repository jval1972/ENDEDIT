//------------------------------------------------------------------------------
//
//  ENDEDIT: An EDNTEXT Editor
//  Copyright (C) 2021 by Jim Valavanis
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
  Menus, ImgList, jpeg, StdCtrls, ee_undo, ee_filemenuhistory, ee_screen;

const
  MINZOOM = 0;
  MAXZOOM = 20;

const
  MOUSEWHEELTIMEOUT = 100; // Msecs until next mouse wheel even to be proccessed

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
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
    ForegroundPalettePanel1: TPanel;
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
    ForegroundPalette1: TImage;
    BackgroundPalettePanel1: TPanel;
    BackgroundPalette1: TImage;
    ToolPanel: TPanel;
    FreeDrawSpeedButton: TSpeedButton;
    FloodFillSpeedButton: TSpeedButton;
    ColorPickerSpeedButton: TSpeedButton;
    EraseSpeedButton: TSpeedButton;
    ElevateSpeedButton: TSpeedButton;
    PaletteSpeedButton1: TSpeedButton;
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
  private
    { Private declarations }
    buffer: TBitmap;
    drawbuffer: TBitmap;
    changed: boolean;
    needsupdate: boolean;
    undoManager: TUndoRedoManager;
    filemenuhistory: TFileMenuHistory;
    ffilename: string;
    escreen: TEndScreen;
    zoom: integer;
    flastzoomwheel: int64;
    blink: boolean;
    lmousedown: boolean;
    lmousedownx, lmousedowny: integer;
    lmousemovex, lmousemovey: integer;
    procedure Idle(Sender: TObject; var Done: Boolean);
    procedure Hint(Sender: TObject);
    procedure UpdateEnable;
    procedure InvalidatePaintBox;
    procedure DrawGrid;
    function ZoomValueX(const X: Integer): Integer;
    function ZoomValueY(const Y: Integer): Integer;
    procedure LLeftMousePaintAt(const X, Y: integer);
    procedure LLeftMousePaintTo(const X, Y: integer);
    procedure CreateDrawBuffer;
    procedure DoCreateNew;
    procedure DoLoadFromStream(const s: TStream);
    procedure DoSaveToStream(const s: TStream);
    procedure DoLoadUndo(const s: TStream);
    procedure DoSaveUndo(const s: TStream);
    function DoLoadFromFile(const aname: string): boolean;
    procedure DoSaveToFile(const aname: string);
    procedure OnLoadFileMenuHistory(Sender: TObject; const aname: string);
    function CheckCanClose: boolean;
    procedure SetFileName(const fname: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  ee_utils, ee_defs;

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

  lmousedown := False;
  lmousedownx := 0;
  lmousedowny := 0;
  lmousemovex := 0;
  lmousemovey := 0;

  buffer := TBitmap.Create;
  buffer.Width := 640;
  buffer.Height := 200;
  buffer.PixelFormat := pf32bit;

  drawbuffer := TBitmap.Create;
  drawbuffer.Width := 640;
  drawbuffer.Height := 400;
  drawbuffer.PixelFormat := pf32bit;

  flastzoomwheel := GetTickCount;

  escreen := TEndScreen.Create;

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

  doCreate := True;
  if ParamCount > 0 then
    if DoLoadFromFile(ParamStr(1)) then
      doCreate := False;

  if docreate then
  begin
    SetFileName('');
    changed := False;
    needsupdate := True;
    undoManager.Clear;
  end
  else
    DoCreateNew;

  Application.OnIdle := Idle;
  Application.OnHint := Hint;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, drawbuffer);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
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
  ee_SaveSettingsToFile(ChangeFileExt(ParamStr(0), '.ini'));

  filemenuhistory.Free;

  buffer.Free;
  drawbuffer.Free;

  escreen.Free;
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
  Paste1.Enabled := Clipboard.HasFormat(CF_BITMAP);
  PasteSpeedButton1.Enabled := Clipboard.HasFormat(CF_BITMAP);
  ZoomInSpeedButton1.Enabled := zoom < MAXZOOM;
  ZoomOutSpeedButton1.Enabled := zoom > MINZOOM;
  if needsupdate then
  begin
    InvalidatePaintBox;
    needsupdate := False;
  end;
end;

procedure TForm1.Paste1Click(Sender: TObject);
begin
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    buffer.LoadFromClipboardFormat(CF_BITMAP, ClipBoard.GetAsHandle(cf_Bitmap), 0);
    InvalidatePaintBox;
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
  Clipboard.Assign(buffer);
end;

procedure TForm1.Open1Click(Sender: TObject);
begin
  if not CheckCanClose then
    Exit;

  if OpenDialog1.Execute then
    DoLoadFromFile(OpenDialog1.FileName);
end;

procedure TForm1.InvalidatePaintBox;
begin
  CreateDrawBuffer;
  PaintBox1.Width := drawbuffer.Width;
  PaintBox1.Height := drawbuffer.Height;
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
  Paste1.Enabled := Clipboard.HasFormat(CF_BITMAP);
  Undo1.Enabled := undoManager.CanUndo;
  Redo1.Enabled := undoManager.CanRedo;
end;

procedure TForm1.LLeftMousePaintAt(const X, Y: integer);
begin
  if not lmousedown then
    Exit;
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

  InvalidatePaintBox;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if button = mbLeft then
  begin
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
  if lmousedown then
  begin
    lmousemovex := ZoomValueX(X);
    lmousemovey := ZoomValueY(Y);
    LLeftMousePaintTo(lmousemovex, lmousemovey);
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

function TForm1.ZoomValueX(const X: Integer): Integer;
begin
  Result := GetIntInRange(X div (drawbuffer.Width div SCREENSIZEX), 0, SCREENSIZEX - 1);
end;

function TForm1.ZoomValueY(const Y: Integer): Integer;
begin
  Result := GetIntInRange(Y div (drawbuffer.Width div SCREENSIZEY), 0, SCREENSIZEY - 1);
end;

procedure TForm1.CreateDrawBuffer;
begin
  escreen.GetBitmap(buffer, blink);
  drawbuffer.Width := 640 + SCREENSIZEX * zoom;
  drawbuffer.Height := 400 + (2 * SCREENSIZEY) * zoom;

  drawbuffer.Canvas.StretchDraw(Rect(0, 0, drawbuffer.Width, drawbuffer.Height), buffer);
  DrawGrid;
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

procedure TForm1.DoCreateNew;
begin
  escreen.Clear;
  undoManager.Clear;
  blink := False;
  SetFileName('');
  changed := False;
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

  undoManager.Clear;
  blink := False;
  Result := True;
  fs := TFileStream.Create(aname, fmOpenRead);
  try
    DoLoadFromStream(fs);
    SetFileName(aname);
    filemenuhistory.AddPath(aname);
    changed := False;
  finally
    fs.Free;
  end;
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
    DoLoadFromFile(aname);
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

end.

