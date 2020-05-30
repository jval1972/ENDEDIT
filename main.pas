//------------------------------------------------------------------------------
//
//  DD_TEXTURE: A tool for creating textures from real world photos.
//  Copyright (C) 2017 by Jim Valavanis
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
//  Site  : https://sourceforge.net/projects/texture-perspective/
//------------------------------------------------------------------------------

unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Buttons, Clipbrd, ExtDlgs, pngimage, xTGA, zBitmap,
  Menus, ImgList, jpeg, StdCtrls;

type
  mark_t = record
    point: TPoint;
    selected: Boolean;
    visible: Boolean;
    button: TSpeedButton;
    nextbutton: TSpeedButton;
    fx, fy: Single;
    fu, fv: Single;
  end;
  mark_p = ^mark_t;

const
  MAXGRID = 34;

type
  markgrid_t = array[0..MAXGRID - 1, 0..MAXGRID - 1] of mark_t;
  markgrid_p = ^markgrid_t;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    OpenButton1: TSpeedButton;
    PasteButton: TSpeedButton;
    Save3dButton: TSpeedButton;
    Copy3dButton: TSpeedButton;
    GridButton1: TSpeedButton;
    SavePictureDialog1: TSavePictureDialog;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Open1: TMenuItem;
    SaveAs1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    Edit1: TMenuItem;
    Copy1: TMenuItem;
    CopyTexture1: TMenuItem;
    N2: TMenuItem;
    Paste1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ImageList1: TImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    ToolPanel: TPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Timer1: TTimer;
    GridTrackBar: TTrackBar;
    GridLabel: TLabel;
    ResetSpeedButton: TSpeedButton;
    Panel4: TPanel;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    Panel5: TPanel;
    ScrollBox2: TScrollBox;
    OpenGLPanel: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    ImagePopupMenu: TPopupMenu;
    TexturePopupMenu: TPopupMenu;
    Copy2: TMenuItem;
    Save1: TMenuItem;
    Open2: TMenuItem;
    Copy3: TMenuItem;
    Paste2: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure OpenGLPanelResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PasteButtonClick(Sender: TObject);
    procedure Copy3dButtonClick(Sender: TObject);
    procedure Save3dButtonClick(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure OpenButton1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Edit1Click(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure GridTrackBarChange(Sender: TObject);
    procedure ResetSpeedButtonClick(Sender: TObject);
  private
    { Private declarations }
    glneedrecalc: boolean;
    buffer: TBitmap;
    drawbuffer: TBitmap;
    rc: HGLRC;   // Rendering Context
    dc: HDC;     // Device Context
    lastglHorzPos, lastglVertPos: integer;
    MARKS: array[0..3] of mark_t;
    GRID: markgrid_t;
    solidpenstyle: boolean;
    mousedown: boolean;
    gridsize: integer;
    procedure Idle(Sender: TObject; var Done: Boolean);
    procedure Hint(Sender: TObject);
    procedure DoRenderGL;
    procedure CreateGLTexture;
    procedure UpdateEnable;
    procedure Get3dPreviewBitmap(const b: TBitmap);
    procedure InvalidatePaintBox;
    procedure TestImage;
    procedure ResetMarks;
    procedure PaintBox1Responer(const X, Y: Integer);
    procedure UpdateMarksFromButtons;
    procedure CreateGrid;
    procedure CreateDrawBuffer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  dglOpenGL,
  tx_utils,
  tx_gl,
  tx_glutils;

procedure TForm1.FormCreate(Sender: TObject);
var
  pfd: TPIXELFORMATDESCRIPTOR;
  pf: Integer;
begin
  Scaled := False;

  solidpenstyle := True;
  mousedown := False;
  gridsize := 9;
  GridTrackBar.Position := gridsize;
  GridLabel.Caption := Format('Grid size = %d', [gridsize - 1]);
  GridTrackBar.Min := 2;
  GridTrackBar.Max := MAXGRID - 1;


  buffer := TBitmap.Create;
  drawbuffer := TBitmap.Create;

  InitOpenGL;
  ReadExtensions;
  ReadImplementationProperties;

  // OpenGL initialisieren
  dc := GetDC(OpenGLPanel.Handle);

  // PixelFormat
  pfd.nSize := SizeOf(pfd);
  pfd.nVersion := 1;
  pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER or 0;
  pfd.iPixelType := PFD_TYPE_RGBA;      // PFD_TYPE_RGBA or PFD_TYPEINDEX
  pfd.cColorBits := 32;

  pf := ChoosePixelFormat(dc, @pfd);   // Returns format that most closely matches above pixel format
  SetPixelFormat(dc, pf, @pfd);

  rc := wglCreateContext(dc);    // Rendering Context = window-glCreateContext
  wglMakeCurrent(dc, rc);        // Make the DC (Form1) the rendering Context

  // Initialize GL environment variables

  glInit(OpenGLPanel.Width);

  OpenGLPanelResize(sender);    // sets up the perspective

  TestImage;

  glneedrecalc := True;
  Application.OnIdle := Idle;
  Application.OnHint := Hint;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, drawbuffer);
end;

procedure TForm1.OpenGLPanelResize(Sender: TObject);
begin
  glViewport(0, 0, OpenGLPanel.Width, OpenGLPanel.Height);    // Set the viewport for the OpenGL window
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity;                     // Reset View
  gluPerspective(45.0, OpenGLPanel.Width/OpenGLPanel.Height, 1.0, 500.0);  // Do the perspective calculations. Last value = max clipping depth

  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glneedrecalc := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  buffer.Free;
  drawbuffer.Free;
  gld_ShutDownTexture;
  wglMakeCurrent(0, 0);
  wglDeleteContext(rc);
end;

procedure TForm1.Idle(Sender: TObject; var Done: Boolean);
var
  newglHorzPos, newglVertPos: integer;
begin
  newglHorzPos := ScrollBox1.HorzScrollBar.Position;
  newglVertPos := ScrollBox1.VertScrollBar.Position;
  if (newglHorzPos <> lastglHorzPos) or (newglVertPos <> lastglVertPos) then
  begin
    lastglVertPos := newglVertPos;
    lastglHorzPos := newglHorzPos;
    glneedrecalc := True;
  end;

  if not glneedrecalc then
    Exit; // jval: We don't need to render :)

  Sleep(1); // Avoid CPU utilization
  DoRenderGL;

  Done := False;
end;

procedure TForm1.DoRenderGL;
var
  w, h: integer;
  x, y: integer;
begin
  w := buffer.Width;
  h := buffer.Height;

  glBeginScene(OpenGLPanel.Width, OpenGLPanel.Height);

  glEnable(GL_TEXTURE_2D);

  glBindTexture(GL_TEXTURE_2D, bitmaptexture);
  glColor3f(1.0, 1.0, 1.0);

  if gridsize <= 1 then
  begin
    glBegin(GL_QUADS);

      glTexCoord2f(MARKS[0].point.X / w, MARKS[0].point.Y / h);
      glVertex2f(MARKS[0].fx, MARKS[0].fy);

      glTexCoord2f(MARKS[1].point.X / w, MARKS[1].point.Y / h);
      glVertex2f(MARKS[1].fx, MARKS[1].fy);

      glTexCoord2f(MARKS[2].point.X / w, MARKS[2].point.Y / h);
      glVertex2f(MARKS[2].fx, MARKS[2].fy);

      glTexCoord2f(MARKS[3].point.X / w, MARKS[3].point.Y / h);
      glVertex2f(MARKS[3].fx, MARKS[3].fy);

    glEnd;
  end
  else
  begin
    glBegin(GL_QUADS);

      for x := 0 to gridsize - 2 do
        for y := 0 to gridsize - 2 do
        begin
          glTexCoord2f(GRID[x, y].fu / w, GRID[x, y].fv / h);
          glVertex2f(GRID[x, y].fx, GRID[x, y].fy);

          glTexCoord2f(GRID[x + 1, y].fu / w, GRID[x + 1, y].fv / h);
          glVertex2f(GRID[x + 1, y].fx, GRID[x + 1, y].fy);

          glTexCoord2f(GRID[x + 1, y + 1].fu / w, GRID[x + 1, y + 1].fv / h);
          glVertex2f(GRID[x + 1, y + 1].fx, GRID[x + 1, y + 1].fy);

          glTexCoord2f(GRID[x, y + 1].fu / w, GRID[x, y + 1].fv / h);
          glVertex2f(GRID[x, y + 1].fx, GRID[x, y + 1].fy);
        end;

    glEnd;
  end;

  glEndScene(dc);
end;

procedure TForm1.UpdateEnable;
begin
  PasteButton.Enabled := Clipboard.HasFormat(CF_BITMAP);
end;

procedure TForm1.PasteButtonClick(Sender: TObject);
begin
  if Clipboard.HasFormat(CF_BITMAP) then
  begin
    buffer.LoadFromClipboardFormat(CF_BITMAP, ClipBoard.GetAsHandle(cf_Bitmap), 0);
    CreateGLTexture;
    ResetMarks;
    InvalidatePaintBox;
    glneedrecalc := True;
  end;
end;

procedure TForm1.Hint(Sender: TObject);
begin
  StatusBar1.SimpleText := Application.Hint;
end;

procedure TForm1.Copy3dButtonClick(Sender: TObject);
var
  b: TBitmap;
begin
  b := TBitmap.Create;
  try
    DoRenderGL; // JVAL: For some unknown reason this must be called before glReadPixels
    Get3dPreviewBitmap(b);
    Clipboard.Assign(b);
  finally
    b.Free;
  end;
end;

procedure TForm1.Save3dButtonClick(Sender: TObject);
var
  b: TBitmap;
begin
  if SavePictureDialog1.Execute then
  begin
    b := TBitmap.Create;
    try
      DoRenderGL; // JVAL: For some unknown reason this must be called before glReadPixels
      Get3dPreviewBitmap(b);
      b.SaveToFile(SavePictureDialog1.FileName);
    finally
      b.Free;
    end;
  end;
end;

procedure TForm1.Get3dPreviewBitmap(const b: TBitmap);
type
  long_a = array[0..$FFFF] of LongWord;
  Plong_a = ^long_a;
var
  L, buf: Plong_a;
  w, h: integer;
  i, j: integer;
  idx: integer;
begin
  w := OpenGLPanel.Width;
  h := OpenGLPanel.Height;
  b.Width := w;
  b.Height := h;
  b.PixelFormat := pf32bit;

  GetMem(L, w * h * SizeOf(LongWord));
  glReadPixels(0, 0, w, h, GL_BGRA, GL_UNSIGNED_BYTE, L);

  idx := 0;
  for j := 0 to h - 1 do
  begin
    buf := b.ScanLine[h - j - 1];
    for i := 0 to w - 1 do
    begin
      buf[i] := L[idx];
      Inc(idx);
    end;
  end;

  FreeMem(L, w * h * SizeOf(LongWord));
end;

resourcestring
  rsTitle = 'Texture Perpective';

procedure TForm1.About1Click(Sender: TObject);
begin
  MessageBox(
    Handle,
    PChar(Format('%s'#13#10'Version %s'#13#10#13#10'A tool for creating textures from real world photos.'#13#10'© 2017-2020, jvalavanis@gmail.com', [rsTitle, I_VersionBuilt])),
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

procedure TForm1.OpenButton1Click(Sender: TObject);
var
  p: TPicture;
begin
  if OpenPictureDialog1.Execute then
  begin
    p := TPicture.Create;
    try
      p.LoadFromFile(OpenPictureDialog1.FileName);
      buffer.PixelFormat := pf32bit;
      if p.Graphic.Width <> 0 then
      begin
        buffer.Width := p.Graphic.Width;
        buffer.Height := p.Graphic.Height;
        buffer.Canvas.Draw(0, 0, p.Graphic)
      end
      else
      begin
        buffer.Width := p.Bitmap.Width;
        buffer.Height := p.Bitmap.Height;
        buffer.Canvas.Draw(0, 0, p.Bitmap)
      end;
      CreateGLTexture;
    finally
      p.Free;
    end;
    ResetMarks;
    InvalidatePaintBox;
    glneedrecalc := True;
  end;
end;

procedure TForm1.InvalidatePaintBox;
begin
  PaintBox1.Width := buffer.Width;
  PaintBox1.Height := buffer.Height;
  CreateDrawBuffer;
  PaintBox1.Invalidate;
end;

procedure TForm1.TestImage;
var
  i, j: integer;
begin
  buffer.Width := 256;
  buffer.Height := 256;
  buffer.PixelFormat := pf32bit;
  buffer.Canvas.Pen.Style := psClear;
  buffer.Canvas.Pen.Color := $0;
  buffer.Canvas.Brush.Style := bsSolid;
  buffer.Canvas.Brush.Color := 0;
  for j := 0 to 7 do
    for i := 0 to 7 do
    begin
      if Odd(i + j) then
        buffer.Canvas.Brush.Color := $FFFFFF
      else
        buffer.Canvas.Brush.Color := 0;
      buffer.Canvas.FillRect(Rect(i * 32, j * 32, (i + 1) * 32, (j + 1) * 32));
    end;
  CreateGLTexture;
  ResetMarks;
  InvalidatePaintBox;
end;

procedure TForm1.ResetMarks;
var
  w, h: integer;
begin
  w := buffer.Width - 1;
  h := buffer.Height - 1;

  MARKS[0].point.X := 0;
  MARKS[0].point.Y := 0;
  MARKS[0].selected := SpeedButton1.Down;
  MARKS[0].button := SpeedButton1;
  MARKS[0].nextbutton := SpeedButton2;
  MARKS[0].visible := True;
  MARKS[0].fx := -1.0;
  MARKS[0].fy := 1.0;

  MARKS[1].point.X := w;
  MARKS[1].point.Y := 0;
  MARKS[1].selected := SpeedButton2.Down;
  MARKS[1].button := SpeedButton2;
  MARKS[1].nextbutton := SpeedButton3;
  MARKS[1].visible := True;
  MARKS[1].fx := 1.0;
  MARKS[1].fy := 1.0;

  MARKS[2].point.X := w;
  MARKS[2].point.Y := h;
  MARKS[2].selected := SpeedButton3.Down;
  MARKS[2].button := SpeedButton3;
  MARKS[2].nextbutton := SpeedButton4;
  MARKS[2].visible := True;
  MARKS[2].fx := 1.0;
  MARKS[2].fy := -1.0;

  MARKS[3].point.X := 0;
  MARKS[3].point.Y := h;
  MARKS[3].selected := SpeedButton4.Down;
  MARKS[3].button := SpeedButton4;
  MARKS[3].nextbutton := SpeedButton1;
  MARKS[3].visible := True;
  MARKS[3].fx := -1.0;
  MARKS[3].fy := -1.0;

  CreateGrid;
end;

procedure TForm1.UpdateMarksFromButtons;
var
  i: integer;
begin
  for i := Low(MARKS) to High(MARKS) do
  begin
    MARKS[i].selected := MARKS[i].button.Down;
    if MARKS[i].selected then
      MARKS[i].visible := not MARKS[i].visible
    else
      MARKS[i].visible := True;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  solidpenstyle := not solidpenstyle;
  UpdateMarksFromButtons;
  UpdateEnable;
  InvalidatePaintBox;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  Paste1.Enabled := Clipboard.HasFormat(CF_BITMAP);
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
  mindist, dist: single;
  minidx: Integer;
begin
  mousedown := Button in [mbLeft];

  if Button = mbLeft then //mbRight then
{  begin
    for i := Low(MARKS) to High(MARKS) do
      if MARKS[i].selected then
        MARKS[i].nextbutton.Down := True;
    UpdateMarksFromButtons;
  end
  else if Button = mbLeft then   }
  begin
    mindist := 1000000.0;
    minidx := -1;
    for i := Low(MARKS) to High(MARKS) do
    begin
      dist := Sqrt(sqr(MARKS[i].point.X - X) + sqr(MARKS[i].point.Y - Y));
      if dist < mindist then
      begin
        mindist := dist;
        minidx := i;
      end;
    end;
    if minidx >= 0 then
    begin
      MARKS[minidx].button.Down := True;
      UpdateMarksFromButtons;
    end;
  end;

  PaintBox1Responer(X, Y);
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mousedown := False;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  PaintBox1Responer(X, Y);
end;

procedure TForm1.PaintBox1Responer(const X, Y: Integer);
var
  i: integer;
begin
  if mousedown then
  begin
    for i := Low(MARKS) to High(MARKS) do
      if MARKS[i].selected then
      begin
        MARKS[i].point.X := GetIntInRange(X, 0, buffer.Width - 1);
        MARKS[i].point.Y := GetIntInRange(Y, 0, buffer.Height - 1);
        Break;
      end;
    CreateGrid;
    InvalidatePaintBox;
  end;
end;

procedure TForm1.CreateGLTexture;
begin
  gld_CreateTexture(buffer);
end;

function InterpolateInt(const x1, x2, x3, x4: Integer; const w1, w2: Single): Integer;
var
  xA, xB: Single;
begin
  xA := x1 * (1 - w1) + x2 * w1;
  xB := x4 * (1 - w1) + x3 * w1;
  Result := Round(xA * (1 - w2) + xB * w2);
end;

function InterpolateFloat(const x1, x2, x3, x4: Single; const w1, w2: Single): Single;
var
  xA, xB: Single;
begin
  xA := x1 * (1 - w1) + x2 * w1;
  xB := x4 * (1 - w1) + x3 * w1;
  Result := xA * (1 - w2) + xB * w2;
end;


procedure TForm1.CreateGrid;
var
  x, y: integer;
  gmark: mark_p;
  wx, wy: Single;
begin
  gridsize := GetIntInRange(gridsize, 1, MAXGRID - 1);
  for x := 0 to gridsize - 1 do
  begin
    wx := x / (gridsize - 1);
    for y := 0 to gridsize - 1 do
    begin
      wy := y / (gridsize - 1);
      gmark := @GRID[x, y];
      gmark.point.X :=
        InterpolateInt(
          MARKS[0].point.X, MARKS[1].point.X, MARKS[2].point.X, MARKS[3].point.X, wx, wy);
      gmark.point.Y :=
        InterpolateInt(
          MARKS[0].point.Y, MARKS[1].point.Y, MARKS[2].point.Y, MARKS[3].point.Y, wx, wy);
      gmark.fx :=
        InterpolateFloat(
          MARKS[0].fx, MARKS[1].fx, MARKS[2].fx, MARKS[3].fx, wx, wy);
      gmark.fy :=
        InterpolateFloat(
          MARKS[0].fy, MARKS[1].fy, MARKS[2].fy, MARKS[3].fy, wx, wy);
      gmark.fu :=
        InterpolateFloat(
          MARKS[0].point.X, MARKS[1].point.X, MARKS[2].point.X, MARKS[3].point.X, wx, wy);
      gmark.fv :=
        InterpolateFloat(
          MARKS[0].point.Y, MARKS[1].point.Y, MARKS[2].point.Y, MARKS[3].point.Y, wx, wy);
    end;
  end;
end;

procedure TForm1.CreateDrawBuffer;
var
  i, j: integer;
begin
  drawbuffer.Width := buffer.Width;
  drawbuffer.Height := buffer.Height;

  drawbuffer.Canvas.Draw(0, 0, buffer);

  if solidpenstyle then
    drawbuffer.Canvas.Pen.Style := psSolid
  else
    drawbuffer.Canvas.Pen.Style := psDot;

  if GridButton1.Down then
  begin
    drawbuffer.Canvas.Pen.Style := psSolid;
    drawbuffer.Canvas.Pen.Width := 1;
    drawbuffer.Canvas.Pen.Color := RGB(255, 255, 0);
    for i := 1 to gridsize - 2 do
    begin
        drawbuffer.Canvas.MoveTo(GRID[i, 0].point.X, GRID[i, 0].point.Y);
        drawbuffer.Canvas.LineTo(GRID[i, gridsize - 1].point.X, GRID[i, gridsize - 1].point.Y);
    end;
    for j := 1 to gridsize - 2 do
    begin
        drawbuffer.Canvas.MoveTo(GRID[0, j].point.X, GRID[0, j].point.Y);
        drawbuffer.Canvas.LineTo(GRID[gridsize - 1, j].point.X, GRID[gridsize - 1, j].point.Y);
    end;
  end;

  drawbuffer.Canvas.Pen.Width := 3;
  drawbuffer.Canvas.Pen.Color := RGB(0, 255, 0);

  drawbuffer.Canvas.MoveTo(MARKS[High(MARKS)].point.X, MARKS[High(MARKS)].point.Y);
  for i := Low(MARKS) to High(MARKS) do
    drawbuffer.Canvas.LineTo(MARKS[i].point.X, MARKS[i].point.Y);

  for i := Low(MARKS) to High(MARKS) do
    if mousedown or MARKS[i].visible then
      ImageList1.Draw(drawbuffer.Canvas, MARKS[i].point.X - 8, MARKS[i].point.Y - 8, 0);
end;

procedure TForm1.GridTrackBarChange(Sender: TObject);
begin
  gridsize := GridTrackBar.Position;
  GridLabel.Caption := Format('Grid size = %d', [gridsize - 1]);
  CreateGrid;
end;

procedure TForm1.ResetSpeedButtonClick(Sender: TObject);
begin
  ResetMarks;
end;

end.

