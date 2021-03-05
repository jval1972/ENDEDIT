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
//  Text Screen
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/endedit/
//------------------------------------------------------------------------------

unit ee_screen;

interface

uses
  Windows, SysUtils, Classes, Graphics, ee_utils;

const
  NUMFRONTCOLORS = 16;
  NUMBACKCOLORS = 8;

  screencolors: array[0..NUMFRONTCOLORS - 1] of LongWord = (
    0,                            // black
    170 shl 16,                          // blue
    170 shl 8,                    // green
    170 shl 8 + 170 shl 16,              // cyan
    170,                   // red
    170 + 170 shl 16,             // magent
    170 + 85 shl 8,        // brown
    170 + 170 shl 8 + 170 shl 16, // light gray
    85 + 85 shl 8 + 85 shl 16,    // dark gray
    85 + 85 shl 8 + 255 shl 16,   // light blue
    85 + 255 shl 8 + 85 shl 16,   // light green
    85 + 255 shl 8 + 255 shl 16,  // light cyan
    255 + 85 shl 8 + 85 shl 16,   // light red
    255 + 85 shl 8 + 255 shl 16,  // light magenta
    255 + 255 shl 8 + 85 shl 16,  // yellow
    255 + 255 shl 8 + 255 shl 16  // white
  );

  swapscreencolors: array[0..NUMFRONTCOLORS - 1] of LongWord = (
    0,                            // black
    170,                          // blue
    170 shl 8,                    // green
    170 shl 8 + 170,              // cyan
    170 shl 16,                   // red
    170 shl 16 + 170,             // magent
    170 shl 16 + 85 shl 8,        // brown
    170 shl 16 + 170 shl 8 + 170, // light gray
    85 shl 16 + 85 shl 8 + 85,    // dark gray
    85 shl 16 + 85 shl 8 + 255,   // light blue
    85 shl 16 + 255 shl 8 + 85,   // light green
    85 shl 16 + 255 shl 8 + 255,  // light cyan
    255 shl 16 + 85 shl 8 + 85,   // light red
    255 shl 16 + 85 shl 8 + 255,  // light magenta
    255 shl 16 + 255 shl 8 + 85,  // yellow
    255 shl 16 + 255 shl 8 + 255  // white
  );

const
  BKMASK = $70;
  FGMASK = $0F;
  BLINKMASK = $80;

const
  SCREENSIZEX = 80;
  SCREENSIZEY = 25;
  SCREENSIZE = SCREENSIZEX * SCREENSIZEY;

type
  endscreenchar_t = packed record
    code: byte;
    flags: byte;
  end;
  Pendscreenchar_t = ^endscreenchar_t;
  endscreen_t = array[0..SCREENSIZE - 1] of endscreenchar_t;
  Pendscreen_t = ^endscreen_t;

type
  TEndScreen = class(TObject)
  private
    data: Pendscreen_t;
    function getidx(const x, y: integer): integer;
    function getcoloridx(const c: LongWord; const range: integer): integer;
    function getbgcoloridx(const c: LongWord): integer;
    function getfgcoloridx(const c: LongWord): integer;
  protected
    procedure SetBackgroundColor(x, y: integer; const color: LongWord); virtual;
    function GetBackgroundColor(x, y: integer): LongWord; virtual;
    procedure SetForegroundColor(x, y: integer; const color: LongWord); virtual;
    function GetForegroundColor(x, y: integer): LongWord; virtual;
    procedure SetBlink(x, y: integer; const blink: Boolean); virtual;
    function GetBlink(x, y: integer): Boolean; virtual;
    procedure SetCharacter(x, y: integer; const ch: Char); virtual;
    function GetCharacter(x, y: integer): Char; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure SaveToStream(const strm: TStream);
    procedure LoadFromStream(const strm: TStream);
    procedure GetBitmap(const b: TBitmap; const blink: boolean);
    procedure AssignTo(const ascreen: TEndScreen);
    property BackgroundColor[x, y: integer]: LongWord read GetBackgroundColor write SetBackgroundColor;
    property ForegroundColor[x, y: integer]: LongWord read GetForegroundColor write SetForegroundColor;
    property Blink[x, y: integer]: boolean read GetBlink write SetBlink;
    property Character[x, y: integer]: char read GetCharacter write SetCharacter;
  end;

implementation

uses
  ee_dosfont;

constructor TEndScreen.Create;
begin
  Inherited;
  GetMem(data, SizeOf(endscreen_t));
  Clear;
end;

destructor TEndScreen.Destroy;
begin
  FreeMem(data, SizeOf(endscreen_t));
  Inherited;
end;

procedure TEndScreen.Clear;
begin
  ZeroMemory(data, SizeOf(endscreen_t));
end;

function TEndScreen.getidx(const x, y: integer): integer;
begin
  Result := y * SCREENSIZEX + x;
end;

function TEndScreen.getcoloridx(const c: LongWord; const range: integer): integer;
var
  i: integer;
  dist, mindist: integer;
  r, g, b: integer;
  r1, g1, b1: integer;
begin
  for i := 0 to range - 1 do
    if screencolors[i] = c then
    begin
      Result := i;
      Exit;
    end;

  mindist := MAXINT;
  Result := 0;
  r1 := GetRValue(c);
  g1 := GetGValue(c);
  b1 := GetBValue(c);
  for i := 0 to range - 1 do
  begin
    r := GetRValue(screencolors[i]) - r1;
    g := GetGValue(screencolors[i]) - g1;
    b := GetBValue(screencolors[i]) - b1;
    dist := r * r + g * g + b * b;
    if dist < mindist then
    begin
      mindist := dist;
      Result := i;
    end;
  end;
end;

function TEndScreen.getbgcoloridx(const c: LongWord): integer;
begin
  Result := getcoloridx(c, NUMBACKCOLORS);
end;

function TEndScreen.getfgcoloridx(const c: LongWord): integer;
begin
  Result := getcoloridx(c, NUMFRONTCOLORS);
end;

procedure TEndScreen.SetBackgroundColor(x, y: integer; const color: LongWord);
var
  idx: integer;
  cidx: integer;
begin
  idx := getidx(x, y);
  cidx := getbgcoloridx(color);
  data[idx].flags := data[idx].flags and not BKMASK;
  data[idx].flags := data[idx].flags or (cidx shl 4);
end;

function TEndScreen.GetBackgroundColor(x, y: integer): LongWord;
var
  idx: integer;
begin
  idx := getidx(x, y);
  Result := screencolors[(data[idx].flags and BKMASK) shr 4];
end;

procedure TEndScreen.SetForegroundColor(x, y: integer; const color: LongWord);
var
  idx: integer;
  cidx: integer;
begin
  idx := getidx(x, y);
  cidx := getfgcoloridx(color);
  data[idx].flags := data[idx].flags and not FGMASK;
  data[idx].flags := data[idx].flags or cidx;
end;

function TEndScreen.GetForegroundColor(x, y: integer): LongWord;
var
  idx: integer;
begin
  idx := getidx(x, y);
  Result := screencolors[data[idx].flags and FGMASK];
end;

procedure TEndScreen.SetBlink(x, y: integer; const blink: Boolean);
var
  idx: integer;
begin
  idx := getidx(x, y);
  if blink then
    data[idx].flags := data[idx].flags or BLINKMASK
  else
    data[idx].flags := data[idx].flags and not BLINKMASK;
end;

function TEndScreen.GetBlink(x, y: integer): Boolean;
var
  idx: integer;
begin
  idx := getidx(x, y);
  Result := data[idx].flags and BLINKMASK <> 0;
end;

procedure TEndScreen.SetCharacter(x, y: integer; const ch: Char);
var
  idx: integer;
begin
  idx := getidx(x, y);
  data[idx].code := Ord(ch);
end;

function TEndScreen.GetCharacter(x, y: integer): Char;
var
  idx: integer;
begin
  idx := getidx(x, y);
  Result := Chr(data[idx].code);
end;

procedure TEndScreen.SaveToStream(const strm: TStream);
begin
  strm.Write(data^, SizeOf(endscreen_t));
end;

procedure TEndScreen.LoadFromStream(const strm: TStream);
begin
  strm.Read(data^, SizeOf(endscreen_t));
end;

procedure TEndScreen.GetBitmap(const b: TBitmap; const blink: boolean);
var
  pe_char: Pendscreenchar_t;
  buf: PLongWordArray;
  i: integer;
  doblink: boolean;
  sp, fp: integer; // Screen offset, font offset
  ix, iy: integer;
  x, y: integer;
  cx, cy: integer;
  bcolor, fcolor: LongWord;
  ln, lnsrc: PLongWordArray;
begin
  b.Width := 640;
  b.Height := 200;
  b.PixelFormat := pf32bit;

  GetMem(buf, 640 * 200 * SizeOf(LongWord));

  pe_char := @data[0];
  for i := 0 to SCREENSIZE - 1 do
  begin
    x := i mod 80;
    y := i div 80;
    bcolor := swapscreencolors[(pe_char.flags shr 4) and 7];
    if pe_char.code = 0 then
    begin
      for iy := 0 to 7 do
      begin
        sp := x * 8 + (y * 8 + iy) * 640;
        for ix := 0 to 7 do
        begin
          buf[sp] := bcolor;
          inc(sp);
        end;
      end;
    end
    else
    begin
      fcolor := swapscreencolors[pe_char.flags and 15];
      cx := (pe_char.code - 1) mod 16;
      cy := (pe_char.code - 1) div 16;
      if blink then
        doblink := pe_char.flags shr 7 <> 0
      else
        doblink := false;
      for iy := 0 to 7 do
      begin
        sp := x * 8 + (y * 8 + iy) * 640;
        fp := cx * 8 + (cy * 8 + iy) * 128;
        for ix := 0 to 7 do
        begin
          if DOSFONT[fp] = 0 then
            buf[sp] := bcolor
          else
          begin
            if doblink then
              buf[sp] := bcolor
            else
              buf[sp] := fcolor
          end;
          inc(sp);
          inc(fp);
        end;
      end;
    end;
    inc(pe_char);
  end;

  lnsrc := @buf[0];
  for y := 0 to 199 do
  begin
    ln := b.ScanLine[y];
    for x := 0 to 639 do
      ln[x] := lnsrc[x];
    lnsrc := @lnsrc[640];
  end;

  FreeMem(buf, 640 * 200 * SizeOf(LongWord));
end;

procedure TEndScreen.AssignTo(const ascreen: TEndScreen);
begin
  Move(ascreen.data^, data^, SizeOf(endscreen_t));
end;

end.
