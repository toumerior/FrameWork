unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uGenericDAO, Vcl.StdCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.Oracle, FireDAC.Phys.OracleDef, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, uTiposPrimitivos, Foo, ClassesTeste;

type
  TForm1 = class(TForm)
    btn1: TButton;
    m: TMemo;
    Button3: TButton;
    procedure btn1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  produto: TProduto;
begin
  produto := TProduto.Create;
//  produto.ID_PRODUTO := 1;
//  produto.NOME := 'OI';
//  produto.ID_MARCA := 10;
//  produto.Classe := TObject(produto);
  produto.Nome.Value_Null := True;
  produto.NOME_MARCA := 'TRAMONTINA';

  m.Text := produto.Select;
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  produto: TProduto;
begin
  produto := TProduto.Create;
  produto.ID_PRODUTO := 1;
  produto.NOME := 'OI';
  produto.ID_MARCA := 10;

  m.Text := produto.Insert;
end;

end.
