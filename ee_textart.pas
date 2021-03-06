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
    ta_diher,
    ta_specialchars
  );

procedure BitmapToScreen(const bm: TBitmap; const escreen: TEndScreen;
  const method: textartmethod_t);

implementation

uses
  ee_utils;

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
var
  r1, g1, b1: integer;
  r2, g2, b2: integer;
  r3, g3, b3: integer;
  r4, g4, b4: integer;
  cc: LongWord;
  mindist: integer;
  rr, gg, bb: integer;
  idx: integer;
  i, j, k: integer;
  dlist: TStringList;
  ch: char;
  w1, w2: single;
  ccd: LongWord;
  dr1, dg1, db1: integer;
  dr2, dg2, db2: integer;
  bkcolor, fgcolor: LongWord;
  IC: TColorItemClass;
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

  if method = ta_diher then
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
    b.Canvas.StretchDraw(Rect(0, 0, b.Width, b.Height), bm);
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
