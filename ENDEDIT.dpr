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
//  Project file
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/endedit/
//------------------------------------------------------------------------------

program ENDEDIT;

uses
  FastMM4 in 'FastMM4.pas',
  FastMM4Messages in 'FastMM4Messages.pas',
  Forms,
  main in 'main.pas' {Form1},
  pngextra in 'pngextra.pas',
  pngimage in 'pngimage.pas',
  pnglang in 'pnglang.pas',
  xTGA in 'xTGA.pas',
  zBitmap in 'zBitmap.pas',
  zlibpas in 'zlibpas.pas',
  ee_utils in 'ee_utils.pas',
  ee_binary in 'ee_binary.pas',
  ee_filemenuhistory in 'ee_filemenuhistory.pas',
  ee_undo in 'ee_undo.pas',
  ee_defs in 'ee_defs.pas',
  ee_screen in 'ee_screen.pas',
  ee_dosfont in 'ee_dosfont.pas',
  ee_textart in 'ee_textart.pas',
  GR32 in 'Graphics32\GR32.pas',
  GR32_ArrowHeads in 'Graphics32\GR32_ArrowHeads.pas',
  GR32_Backends in 'Graphics32\GR32_Backends.pas',
  GR32_Backends_Generic in 'Graphics32\GR32_Backends_Generic.pas',
  GR32_Backends_VCL in 'Graphics32\GR32_Backends_VCL.pas',
  GR32_Bindings in 'Graphics32\GR32_Bindings.pas',
  GR32_Blend in 'Graphics32\GR32_Blend.pas',
  GR32_BlendASM in 'Graphics32\GR32_BlendASM.pas',
  GR32_BlendMMX in 'Graphics32\GR32_BlendMMX.pas',
  GR32_BlendSSE2 in 'Graphics32\GR32_BlendSSE2.pas',
  GR32_Blurs in 'Graphics32\GR32_Blurs.pas',
  GR32_Brushes in 'Graphics32\GR32_Brushes.pas',
  GR32_Clipper in 'Graphics32\GR32_Clipper.pas',
  GR32_ColorGradients in 'Graphics32\GR32_ColorGradients.pas',
  GR32_ColorPicker in 'Graphics32\GR32_ColorPicker.pas',
  GR32_ColorSwatch in 'Graphics32\GR32_ColorSwatch.pas',
  GR32_Containers in 'Graphics32\GR32_Containers.pas',
  GR32_ExtImage in 'Graphics32\GR32_ExtImage.pas',
  GR32_Filters in 'Graphics32\GR32_Filters.pas',
  GR32_Gamma in 'Graphics32\GR32_Gamma.pas',
  GR32_Geometry in 'Graphics32\GR32_Geometry.pas',
  GR32_Image in 'Graphics32\GR32_Image.pas',
  GR32_Layers in 'Graphics32\GR32_Layers.pas',
  GR32_LowLevel in 'Graphics32\GR32_LowLevel.pas',
  GR32_Math in 'Graphics32\GR32_Math.pas',
  GR32_MicroTiles in 'Graphics32\GR32_MicroTiles.pas',
  GR32_OrdinalMaps in 'Graphics32\GR32_OrdinalMaps.pas',
  GR32_Paths in 'Graphics32\GR32_Paths.pas',
  GR32_Polygons in 'Graphics32\GR32_Polygons.pas',
  GR32_PolygonsAggLite in 'Graphics32\GR32_PolygonsAggLite.pas',
  GR32_RangeBars in 'Graphics32\GR32_RangeBars.pas',
  GR32_Rasterizers in 'Graphics32\GR32_Rasterizers.pas',
  GR32_RepaintOpt in 'Graphics32\GR32_RepaintOpt.pas',
  GR32_Resamplers in 'Graphics32\GR32_Resamplers.pas',
  GR32_System in 'Graphics32\GR32_System.pas',
  GR32_Text_VCL in 'Graphics32\GR32_Text_VCL.pas',
  GR32_Transforms in 'Graphics32\GR32_Transforms.pas',
  GR32_VectorMaps in 'Graphics32\GR32_VectorMaps.pas',
  GR32_VectorUtils in 'Graphics32\GR32_VectorUtils.pas',
  GR32_VPR in 'Graphics32\GR32_VPR.pas',
  GR32_VPR2 in 'Graphics32\GR32_VPR2.pas',
  GR32_XPThemes in 'Graphics32\GR32_XPThemes.pas',
  frm_imgconvertmethod in 'frm_imgconvertmethod.pas' {ImgConvertMethodForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ENDTEXT Editor';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
