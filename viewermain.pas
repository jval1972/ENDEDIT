unit viewermain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ee_screen;

type
  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1DblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
    screen: TEndScreen;
    screenloaded: boolean;
    blink: boolean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

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
begin
  PaintBox1.Align := alClient;
  screen := TEndScreen.Create;

  screenloaded := False;

  if ParamCount < 1 then
    Exit;

  DoubleBuffered := True;
  blink := False;
  
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

end.
