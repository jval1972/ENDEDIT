//------------------------------------------------------------------------------
//
//  ENDVIEW: An ENDTEXT Viewer
//  Copyright (C) 2021-2023 by Jim Valavanis
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

unit viewermain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ee_screen, StdCtrls;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1DblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Memo1DblClick(Sender: TObject);
  private
    { Private declarations }
    screen: TEndScreen;
    screenloaded: boolean;
    blink: boolean;
    mpoint: TPoint;
    mousedown: boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override; // ADD THIS LINE!
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  ev_wadreader;

function findparam(const parm: string): integer;
var
  i: integer;
begin
  for i := 1 to ParamCount do
    if ParamStr(i) = parm then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  x: integer;
  f: TFileStream;
  src: Pointer;
  ms: TMemoryStream;
  wad: TWadReader;
  entry: string;
  id: integer;
  buf: pointer;
  bufsize: integer;
begin
  PaintBox1.Align := alClient;
  screen := TEndScreen.Create;

  screenloaded := False;

  if findparam('-fullscreen') > 0 then
    windowstate := wsMaximized;
  x := findparam('-width');
  if (x > 0) and (x < ParamCount) then
    Width := StrToIntDef(ParamStr(x + 1), Width);
  x := findparam('-height');
  if (x > 0) and (x < ParamCount) then
    Height := StrToIntDef(ParamStr(x + 1), Height);
  x := findparam('-top');
  if (x > 0) and (x < ParamCount) then
    Top := StrToIntDef(ParamStr(x + 1), Top);
  x := findparam('-left');
  if (x > 0) and (x < ParamCount) then
    Left := StrToIntDef(ParamStr(x + 1), Left);

  DoubleBuffered := True;
  blink := False;
  mpoint.X := 0;
  mpoint.Y := 0;
  mousedown := False;

  x := findparam('-file');
  if (x > 0) and (x < ParamCount) then
  begin
    if FileExists(ParamStr(x + 1)) then
    begin
      f := TFileStream.Create(ParamStr(x + 1), fmOpenRead or fmShareDenyWrite);
      try
        screen.LoadFromStream(f);
        screenloaded := True;
      finally
        f.Free;
      end;
    end;
  end;
  if screenloaded then
    Exit;

  x := findparam('-buffer');
  if (x > 0) and (x < ParamCount) then
  begin
    src := Pointer(StrToIntDef(ParamStr(x + 1), 0));
    if Integer(src) > 0 then
    begin
      ms := TMemoryStream.Create;
      try
        ms.Write(src^, SizeOf(endscreen_t));
        ms.Position := 0;
        screen.LoadFromStream(ms);
        screenloaded := True;
      finally
        ms.Free;
      end;
    end;
  end;
  if screenloaded then
    Exit;

  x := findparam('-wadfile');
  if (x > 0) and (x < ParamCount) then
  begin
    if FileExists(ParamStr(x + 1)) then
    begin
      wad := TWadReader.Create;
      wad.OpenWadFile(ParamStr(x + 1));
      x := findparam('-entry');
      if (x > 0) and (x < ParamCount) then
        entry := ParamStr(x + 1)
      else if wad.EntryId('ENDOOM') >= 0 then
        entry := 'ENDOOM'
      else if wad.EntryId('ENDTEXT') >= 0 then
        entry := 'ENDTEXT'
      else if wad.EntryId('ENDSTRF') >= 0 then
        entry := 'ENDSTRF'
      else
        entry := '';
      if entry <> '' then
      begin
        id := wad.EntryId(entry);
        if id >= 0 then
          if wad.ReadEntry(id, buf, bufsize) then
          begin
            if bufsize >= SizeOf(endscreen_t) then
            begin
              ms := TMemoryStream.Create;
              try
                ms.Write(buf^, SizeOf(endscreen_t));
                ms.Position := 0;
                screen.LoadFromStream(ms);
                screenloaded := True;
              finally
                ms.Free;
              end;
            end;
            FreeMem(buf, bufsize);
          end;
      end;
      wad.Free;
    end;
  end;

  if not screenloaded then
  begin
    Paintbox1.Visible := False;
    Memo1.Visible := True;
    Memo1.Align := alClient;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  screen.Free;
end;

procedure TForm1.PaintBox1DblClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  Close;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
var
  bm: TBitmap;
begin
  bm := TBitmap.Create;
  screen.GetBitmap(bm, blink);
  PaintBox1.Canvas.StretchDraw(Rect(0, 0, PaintBox1.Width, PaintBox1.Height), bm);
  bm.Free;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  blink := not blink;
  PaintBox1.Invalidate;
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
begin
  p.X := X;
  p.Y := Y;
  mpoint := ClientToScreen(p);
  mousedown := True;
end;

procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  p: TPoint;
  dx, dy: integer;
begin
  if mousedown then
  begin
    p.X := X;
    p.Y := Y;
    p := ClientToScreen(p);
    dx := mpoint.X - p.X;
    dy := mpoint.Y - p.Y;
    Left := Left - dx;
    Top := Top - dy;
    mpoint := p;
  end;
end;

procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mousedown := False;
end;

procedure TForm1.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Params.Style := Params.Style or WS_THICKFRAME;
end;

procedure TForm1.Memo1DblClick(Sender: TObject);
begin
  Close
end;

end.
