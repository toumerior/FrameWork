unit uAluno;

interface

uses
  uEntity,  uAtribEntity;

type
//nome da classe de entidade
  [TableName('TB_CAD_ALUNO')]
  TAluno = class(TGenericEntity)
  private
    //colocar aqui todas as propriedades privadas da classe de acordo com
    //os campos da tabela
    FMatricula:string;
    FNomeAluno:string;
    FEndereco:string;
    FTelefone:string;
    FCPF:string;

  //colocar aqui todos os métodos sets para cada uma das propriedades acima
    procedure setFMatricula(value:string);
    procedure setFNomeAluno(value:string);
    procedure setFEndereco(value:string);
    procedure setFTelefone(value:string);
    procedure setFCPF(value:string);

  public
    //colocar aqui todas as propriedades públicas de acesso aos objetos dessa
    //classe
    [KeyField('MATRICULA')]
    [FieldName('MATRICULA')]
    property Matricula: string read FMatricula write setFMatricula;

    [FieldName('NOME_ALUNO')]
    property Nome:string read FNomeAluno write setFNomeAluno;

    [FieldName('ENDERECO')]
    property Endereco:string read FEndereco write setFEndereco;

    [FieldName('TELEFONE')]
    property Telefone:string read FTelefone write setFTelefone;

    [FieldName('CPF')]
    property CPF:string read FCPF write setFCPF;

    function ToString:string; override;
end;

implementation

procedure TAluno.setFMatricula(value:string);
begin
  FMatricula:= value;
end;

procedure TAluno.setFNomeAluno(value:string);
begin
  FNomeAluno:= value;
end;

procedure TAluno.setFEndereco(value:string);
begin
  FEndereco:= value;
end;

procedure TAluno.setFTelefone(value:string);
begin
  FTelefone:= value;
end;

procedure TAluno.setFCPF(value:string);
begin
  FCPF:= value;
end;

function TAluno.toString;
begin
  Result := ' Matricula: '+ Matricula + ' Nome: ' + Nome + ' Endereco: ' + Endereco +
            ' Fone: ' + Telefone + ' CPF: ' + CPF;
end;

end.
