//------------------------------------------------------------------------------
//
//  ENDEDIT: An ENDTEXT Editor
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
//  Text Art (Convert image to Text Screen)
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/endedit/
//------------------------------------------------------------------------------

unit ee_textart;

interface

uses
  Windows, SysUtils, Classes, Graphics, ee_screen;

type
  textartmethod_t = (
    ta_background_only,
    ta_background_foreground,
    ta_dither,
    ta_specialchars,
    ta_extendedspecialchars,
    NUMTEXTARTMETHOD
  );

const
  TEXTMETHODNAMES: array[0..Ord(NUMTEXTARTMETHOD) - 1] of string[64] = (
    'Background only',
    'Background & Foreground',
    'Dithering',
    'Use Special Characters',
    'Use Extended Special Characters'
  );

procedure BitmapToScreen(const bm: TBitmap; const escreen: TEndScreen;
  const method: textartmethod_t);

implementation

uses
  ee_utils,
  GR32_Image,
  GR32_System,
  GR32_RangeBars,
  GR32,
  GR32_Resamplers;

procedure FlipBitmapVertical(const b: TBitmap);
var
  i, j: integer;
  tmp: LongWord;
  l1, l2: PLongWordArray;
begin
  b.PixelFormat := pf32bit;

  for j := 0 to b.Height div 2 - 1 do
  begin
    l1 := b.ScanLine[j];
    l2 := b.ScanLine[b.Height - 1 - j];
    for i := 0 to b.Width - 1 do
    begin
      tmp := l1[i];
      l1[i] := l2[i];
      l2[i] := tmp;
    end;
  end;
end;

procedure BitmapLanczosDraw(const bm: TBitmap; const destbm: TBitmap);
var
  R: TKernelResampler;
  Src, Dst: TBitmap32;
  bmp: TBitmap;
  bi: TBitmapInfo;
  abitmap: HBitmap;
begin
  Src := TBitmap32.Create;
  Src.Assign(bm);
  Dst := TBitmap32.Create;
  Dst.Assign(destbm);

  R := TKernelResampler.Create(Src);
  R.Kernel := TLanczosKernel.Create;
  Dst.Draw(Dst.BoundsRect, Src.BoundsRect, Src);

  FillChar(bi, SizeOf(bi), 0);
  with bi.bmiHeader do
  begin
     biSize := SizeOf(bi.bmiHeader);
     biWidth := Dst.Width;
     biHeight := Dst.height;
     biPlanes := 1;
     biBitCount := 32;
     biCompression := BI_RGB;
  end;

  bmp := TBitmap.Create;

  aBitmap := 0;
  try
    aBitmap := CreateDIBitmap(GetDC(0), bi.bmiHeader, CBM_INIT,  @Dst.Bits[0], bi, DIB_RGB_COLORS);
    bmp.handle := aBitmap;
    destbm.Assign(bmp);
    destbm.PixelFormat := pf32bit;
  finally
    DeleteObject(aBitmap);
    bmp.Free;
  end;

  FlipBitmapVertical(destbm);
  Src.Free;
  Dst.Free;
end;

type
  coloritem_t = record
    bkcolor: LongWord;
    fgcolor: LongWord;
    ch: Char;
  end;
  Pcoloritem_t = ^coloritem_t;

  TColorItemClass = class(TObject)
    color: LongWord;
    bkcolor: LongWord;
    fgcolor: LongWord;
    ch: Char;
    constructor Create(const acolor, abkcolor, afgcolor: LongWord;
      const ach: Char); virtual;
  end;

constructor TColorItemClass.Create(const acolor, abkcolor, afgcolor: LongWord;
  const ach: Char);
begin
  Inherited Create;
  color := acolor;
  bkcolor := abkcolor;
  fgcolor := afgcolor;
  ch := ach;
end;

procedure GetRGB(const c: LongWord; var r, g, b: integer);
begin
  r := GetRValue(c);
  g := GetGValue(c);
  b := GetBValue(c);
end;

function GetNearestScreenColorIdx(const c: LongWord; const range: integer;
  var mindist: integer): integer;
var
  i: integer;
  dist: integer;
  r, g, b: integer;
  r1, g1, b1: integer;
begin
  for i := 0 to range - 1 do
    if screencolors[i] = c then
    begin
      Result := i;
      mindist := 0;
      Exit;
    end;

  mindist := MAXINT;
  Result := 0;
  GetRGB(c, r1, g1, b1);
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

function GetNearestScreenColorListIdx(const c: LongWord; const lst: TStringList;
  var mindist: integer): integer;
var
  i: integer;
  dist: integer;
  r, g, b: integer;
  r1, g1, b1: integer;
  IC: TColorItemClass;
begin
  mindist := MAXINT;
  Result := 0;
  GetRGB(c, r1, g1, b1);
  for i := 0 to lst.Count - 1 do
  begin
    IC := lst.Objects[i] as TColorItemClass;
    r := GetRValue(IC.color) - r1;
    g := GetGValue(IC.color) - g1;
    b := GetBValue(IC.color) - b1;
    dist := r * r + g * g + b * b;
    if dist < mindist then
    begin
      mindist := dist;
      Result := i;
    end
    else if dist = mindist then
      if IC.ch = ' ' then
        Result := i;  // We prefer blanc/space entries
  end;
end;

procedure NearestColorItem(const c1, c2, c3, c4: LongWord; const item: Pcoloritem_t;
  const method: textartmethod_t);
const
  NUMDIHERELEMENTS = 5;
  DIHERCHARS: array[0..NUMDIHERELEMENTS - 1] of Char = (' ', Chr($B0), Chr($B1), Chr($B2), Chr($DB));
  DIHERWEIGHTS: array[0..NUMDIHERELEMENTS - 1] of single = (1.0, 0.75, 0.5, 0.25, 0.0);
const
  NUMSPECIALCHARS = 6;
  NUMEXTENDEDSPECIALCHARS = NUMSPECIALCHARS + 9;
  SPECIALCHARS: array[0..NUMEXTENDEDSPECIALCHARS - 1] of Char = (
    Chr($DB), Chr($DC), Chr($DD), Chr($DC), Chr($DF), ' ',
    Chr($DA), Chr($BF), Chr($C0), Chr($D9), Chr($C1), Chr($C2), Chr($C3), Chr($B4), Chr($C5)
  );
  SPECIALCHARSROVERS: array[0..NUMEXTENDEDSPECIALCHARS - 1] of string[4] = (
    'FFFF', 'BBFF', 'FBFB', 'BFBF', 'FFBB', 'BBBB',
    'BBBf', 'BBfB', 'BfBB', 'fBBB', 'ffBB', 'BBff', 'BfBf', 'fBfB', 'ffff'
  );
var
  r1, g1, b1: integer;
  r2, g2, b2: integer;
  r3, g3, b3: integer;
  r4, g4, b4: integer;
  cc: LongWord;
  dist, mindist: integer;
  rr, gg, bb: integer;
  idx: integer;
  i, j, k, l: integer;
  dlist: TStringList;
  ch: char;
  w1, w2: single;
  ccd: LongWord;
  dr1, dg1, db1: integer;
  dr2, dg2, db2: integer;
  bkcolor, fgcolor: LongWord;
  IC: TColorItemClass;
  ccs: array[1..4] of LongWord;
  bestbkcolor: LongWord;
  bestfgcolor: LongWord;
  numspecials: integer;
begin
  GetRGB(c1, r1, g1, b1);
  GetRGB(c2, r2, g2, b2);
  GetRGB(c3, r3, g3, b3);
  GetRGB(c4, r4, g4, b4);

  if method = ta_background_only then
  begin
    rr := (r1 + r2 + r3 + r4) div 4;
    gg := (g1 + g2 + g3 + g4) div 4;
    bb := (b1 + b2 + b3 + b4) div 4;
    cc := RGB(rr, gg, bb);
    item.bkcolor := screencolors[GetNearestScreenColorIdx(cc, NUMBACKCOLORS, mindist)];
    Exit;
  end;

  if method = ta_background_foreground then
  begin
    rr := (r1 + r2 + r3 + r4) div 4;
    gg := (g1 + g2 + g3 + g4) div 4;
    bb := (b1 + b2 + b3 + b4) div 4;
    cc := RGB(rr, gg, bb);
    idx := GetNearestScreenColorIdx(cc, NUMFRONTCOLORS, mindist);
    if idx < NUMBACKCOLORS then
    begin
      item.bkcolor := screencolors[idx];
      item.ch := ' ';
    end
    else
    begin
      item.fgcolor := screencolors[idx];
      item.ch := Chr($DB);
    end;
    Exit;
  end;

  if method = ta_dither then
  begin
    rr := (r1 + r2 + r3 + r4) div 4;
    gg := (g1 + g2 + g3 + g4) div 4;
    bb := (b1 + b2 + b3 + b4) div 4;
    cc := RGB(rr, gg, bb);

    dlist := TStringList.Create;
    try
      for i := 0 to NUMBACKCOLORS - 1 do
      begin
        bkcolor := screencolors[i];
        GetRGB(bkcolor, dr1, dg1, db1);
        for j := 0 to NUMFRONTCOLORS - 1 do
          if i <> j then
          begin
            fgcolor := screencolors[j];
            for k := 0 to NUMDIHERELEMENTS - 1 do
            begin
              ch := DIHERCHARS[k];
              w1 := DIHERWEIGHTS[k];
              w2 := 1.0 - w1;
              GetRGB(fgcolor, dr2, dg2, db2);
              dr2 := GetIntInRange(Round(dr1 * w1 + dr2 * w2), 0, 255);
              dg2 := GetIntInRange(Round(dg1 * w1 + dg2 * w2), 0, 255);
              db2 := GetIntInRange(Round(db1 * w1 + db2 * w2), 0, 255);
              ccd := RGB(dr2, dg2, db2);
              dlist.AddObject('-', TColorItemClass.Create(ccd, bkcolor, fgcolor, ch));
            end;
          end;
      end;

      IC := dlist.Objects[GetNearestScreenColorListIdx(cc, dlist, mindist)] as TColorItemClass;
      item.bkcolor := IC.bkcolor;
      item.fgcolor := IC.fgcolor;
      item.ch := IC.ch;

      for i := 0 to dlist.Count - 1 do
        dlist.Objects[i].Free;
    finally
      dlist.Free;
    end;
    Exit;
  end;

  if method in [ta_specialchars, ta_extendedspecialchars] then
  begin
    if method = ta_specialchars then
      numspecials := NUMSPECIALCHARS
    else
      numspecials := NUMEXTENDEDSPECIALCHARS;

    ccs[1] := c1;
    ccs[2] := c2;
    ccs[3] := c3;
    ccs[4] := c4;

    bestbkcolor := screencolors[0];
    bestfgcolor := screencolors[0];
    mindist := MAXINT;
    ch := SPECIALCHARS[0];

    for i := 0 to NUMBACKCOLORS - 1 do
    begin
      bkcolor := screencolors[i];
      GetRGB(bkcolor, dr1, dg1, db1);
      for j := 0 to NUMFRONTCOLORS - 1 do
        if i <> j then
        begin
          fgcolor := screencolors[j];
          GetRGB(fgcolor, dr2, dg2, db2);
          for k := 0 to numspecials - 1 do
          begin
            dist := 0;
            for l := 1 to 4 do
            begin
              GetRGB(ccs[l], r1, g1, b1);
              if SPECIALCHARSROVERS[k][l] = 'F' then
              begin
                r1 := r1 - dr2;
                g1 := g1 - dg2;
                b1 := b1 - db2;
              end
              else if SPECIALCHARSROVERS[k][l] = 'f' then
              begin
                r1 := r1 - (dr1 + dr2) div 2;
                g1 := g1 - (dg1 + dg2) div 2;
                b1 := b1 - (db1 + db2) div 2;
              end
              else
              begin
                r1 := r1 - dr1;
                g1 := g1 - dg1;
                b1 := b1 - db1;
              end;
              dist := dist + r1 * r1 + g1 * g1 + b1 * b1;
            end;

            if (dist < mindist) or ((dist = mindist) and (SPECIALCHARS[k] = ' ')) then
            begin
              mindist := dist;
              bestbkcolor := bkcolor;
              bestfgcolor := fgcolor;
              ch := SPECIALCHARS[k];
            end;
          end;
        end
      end;

      item.bkcolor := bestbkcolor;
      item.fgcolor := bestfgcolor;
      item.ch := ch;
    Exit;
  end;



end;

procedure BitmapToScreen(const bm: TBitmap; const escreen: TEndScreen;
  const method: textartmethod_t);
var
  b: TBitmap;
  x, y: integer;
  item: coloritem_t;
  c1, c2, c3, c4: LongWord;
begin
  b := TBitmap.Create;
  try
    b.Width := SCREENSIZEX * 2;
    b.Height := SCREENSIZEY * 2;
    b.PixelFormat := pf32bit;
    BitmapLanczosDraw(bm, b);
    for x := 0 to SCREENSIZEX - 1 do
      for y := 0 to SCREENSIZEY - 1 do
      begin
        item.bkcolor := escreen.BackgroundColor[x, y];
        item.fgcolor := escreen.ForegroundColor[x, y];
        item.ch := escreen.Character[x, y];
        c1 := b.Canvas.Pixels[x * 2, y * 2];
        c2 := b.Canvas.Pixels[x * 2 + 1, y * 2];
        c3 := b.Canvas.Pixels[x * 2, y * 2 + 1];
        c4 := b.Canvas.Pixels[x * 2 + 1, y * 2 + 1];
        NearestColorItem(c1, c2, c3, c4, @item, method);
        escreen.BackgroundColor[x, y] := item.bkcolor;
        escreen.ForegroundColor[x, y] := item.fgcolor;
        escreen.Character[x, y] := item.ch;
      end;
  finally
    b.Free;
  end;
end;

end.
