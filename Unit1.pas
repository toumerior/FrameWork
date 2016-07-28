unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uAluno, uGenericDAO, Vcl.StdCtrls,
  FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async,
  FireDAC.Phys, FireDAC.Phys.Oracle, FireDAC.Phys.OracleDef, FireDAC.VCLUI.Wait,
  Data.DB, FireDAC.Comp.Client, uTiposPrimitivos, Foo, ClassesTeste;

type
  TForm1 = class(TForm)
    Button1: TButton;
    btn1: TButton;
    m: TMemo;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
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
  aluno: TAluno;
  produto: TProduto;
begin
  produto := TProduto.Create;
//  produto.ID_PRODUTO := 1;
//  produto.NOME := 'OI';
//  produto.ID_MARCA := 10;
//  produto.Classe := TObject(produto);
  produto.NOME.FiltrarNull := True;

  m.Text := produto.SelectBasico;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  aluno: TAluno;
begin
//  aluno := TAluno.Create;
  aluno.Nome := 'BRUNO';
  aluno.Matricula := '0028';
  aluno.Endereco := 'Lugar nenhum';
  aluno.Telefone := '2321';
  aluno.CPF := '04923311118';

//  if TGenericDAO.Insert(aluno) then
//    ShowMessage('Funfou');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
//  x: Anulavel<Integer>;
  x: TInteger;
begin
  x := 2;

end;

end.
