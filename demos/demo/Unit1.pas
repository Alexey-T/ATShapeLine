unit Unit1;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShapeLine, ExtCtrls, StdCtrls;

type
  TForm1 = class(TForm)
    Line2: TShapeLine;
    Line1: TShapeLine;
    Line3: TShapeLine;
    Shape1: TShape;
    Label2: TLabel;
    Line4: TShapeLine;
    Label3: TLabel;
    Label4: TLabel;
    GroupBox1: TGroupBox;
    ColorDialog1: TColorDialog;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton3: TRadioButton;
    Label1: TLabel;
    probe: TShapeLine;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Edit1: TEdit;
    Label11: TLabel;
    procedure Panel1Click(Sender: TObject);
    procedure Panel2Click(Sender: TObject);
    procedure ConfigTLine(Sender: TObject);
    procedure Edit1KeyPress(Sender: TObject; var Key: Char);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.Panel1Click(Sender: TObject);
begin
ColorDialog1.Color:=Probe.LineColor;
if ColorDialog1.Execute then begin
   Probe.LineColor:=ColorDialog1.Color;
   Panel1.Color:=Probe.LineColor;
end;
end;

procedure TForm1.Panel2Click(Sender: TObject);
begin
ColorDialog1.Color:=Probe.ArrowColor;
if ColorDialog1.Execute then begin
   Probe.ArrowColor:=ColorDialog1.Color;
   Panel2.Color:=Probe.ArrowColor;
end;
end;

procedure TForm1.ConfigTLine(Sender: TObject);
begin
Probe.Arrow1:=CheckBox1.Checked;
Probe.Arrow2:=CheckBox2.Checked;
if RadioButton1.Checked then
   Probe.Direction:=drLeftRight
else
if RadioButton2.Checked then
   Probe.Direction:=drUpDown
else
if RadioButton3.Checked then
   Probe.Direction:=drTopLeftBottomRight
else
   Probe.Direction:=drTopRightBottomLeft
end;

procedure TForm1.Edit1KeyPress(Sender: TObject; var Key: Char);
begin
Case Key of
'0'..'9',#13,#8:;
else Key:=#0;
end;
end;

procedure TForm1.Edit1KeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
if(Key=VK_Return)and(Edit1.Text<>'') then
  Probe.ArrowFactor:=StrToInt(Edit1.Text);
end;

end.
