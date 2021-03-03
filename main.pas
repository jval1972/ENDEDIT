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
  Menus, ImgList, jpeg, StdCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    Panel2: TPanel;
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
    N2: TMenuItem;
    Paste1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    OpenPictureDialog1: TOpenPictureDialog;
    ToolPanel: TPanel;
    Timer1: TTimer;
    ImagePopupMenu: TPopupMenu;
    TexturePopupMenu: TPopupMenu;
    Copy2: TMenuItem;
    Save1: TMenuItem;
    Open2: TMenuItem;
    Copy3: TMenuItem;
    Paste2: TMenuItem;
    Panel4: TPanel;
    ScrollBox1: TScrollBox;
    PaintBox1: TPaintBox;
    Panel6: TPanel;
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
    procedure FormCreate(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PasteButtonClick(Sender: TObject);
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
  private
    { Private declarations }
    buffer: TBitmap;
    drawbuffer: TBitmap;
    mousedown: boolean;
    procedure Idle(Sender: TObject; var Done: Boolean);
    procedure Hint(Sender: TObject);
    procedure UpdateEnable;
    procedure InvalidatePaintBox;
    procedure PaintBox1Responer(const X, Y: Integer);
    procedure CreateDrawBuffer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  ee_utils;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Scaled := False;

  mousedown := False;

  buffer := TBitmap.Create;
  drawbuffer := TBitmap.Create;

  Application.OnIdle := Idle;
  Application.OnHint := Hint;
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
  PaintBox1.Canvas.Draw(0, 0, drawbuffer);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  buffer.Free;
  drawbuffer.Free;
end;

procedure TForm1.Idle(Sender: TObject; var Done: Boolean);
begin
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
    PChar(Format('%s'#13#10'Version %s'#13#10#13#10'A tool for creating ENDTEXT screens.'#13#10'© 2021, jvalavanis@gmail.com', [rsTitle, I_VersionBuilt])),
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
    finally
      p.Free;
    end;
    InvalidatePaintBox;
  end;
end;

procedure TForm1.InvalidatePaintBox;
begin
  PaintBox1.Width := buffer.Width;
  PaintBox1.Height := buffer.Height;
  CreateDrawBuffer;
  PaintBox1.Invalidate;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdateEnable;
  InvalidatePaintBox;
end;

procedure TForm1.Edit1Click(Sender: TObject);
begin
  Paste1.Enabled := Clipboard.HasFormat(CF_BITMAP);
end;

procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  mousedown := Button in [mbLeft];
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
begin
  if mousedown then
  begin
    InvalidatePaintBox;
  end;
end;

procedure TForm1.CreateDrawBuffer;
begin
  drawbuffer.Width := buffer.Width;
  drawbuffer.Height := buffer.Height;

  drawbuffer.Canvas.Draw(0, 0, buffer);
end;

end.

