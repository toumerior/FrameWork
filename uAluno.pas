unit uAluno;

interface

uses
  uEntity,  uAtribEntity, System.SysUtils, uGenericDAO, uTiposPrimitivos;

type
//nome da classe de entidade
  [TableName('ALUNOS')]
  TAluno = class(TGenericDAO)
  private
    //colocar aqui todas as propriedades privadas da classe de acordo com
    //os campos da tabela
    FMatricula:Tstring;
    FNomeAluno:Tstring;
    FEndereco:Tstring;
    FTelefone:Tstring;
    FCPF:Tstring;
    FID_PESSOA: TInteger;
    FID_PESSOA_MAIS: TInteger;
    FID_PRODUTO: TInteger;
    FID_NOVO: TInteger;

  //colocar aqui todos os métodos sets para cada uma das propriedades acima
    procedure setFMatricula(value:Tstring);
    procedure setFNomeAluno(value:Tstring);
    procedure setFEndereco(value:Tstring);
    procedure setFTelefone(value:Tstring);
    procedure setFCPF(value:Tstring);
    procedure setID_PESSOA(const Value: TInteger);
    procedure setID_PESSOA_MAIS(const Value: TInteger);
    procedure setID_PRODUTO(const Value: TInteger);
    procedure setID_NOVO(const Value: TInteger);

  public
    constructor Create; override;
    //colocar aqui todas as propriedades públicas de acesso aos objetos dessa
    //classe
    [PropriedadesCampo('ID_PRODUTO', False, True, 'PRODUTOS', 'ID_PRODUTO', 'PRO', Left)]
    property ID_PRODUTO: TInteger read FID_PRODUTO write setID_PRODUTO;

    [KeyField('MATRICULA')]
    [FieldName('MATRICULA')]
    [PropriedadesCampo('MATRICULA', True, True, '', '', '', Inner)]
    property Matricula: Tstring read FMatricula write setFMatricula;

    [FieldName('NOME_ALUNO')]
    [PropriedadesCampo('NOME_ALUNO', False, True, '', '', '', Inner)]
    property Nome:Tstring read FNomeAluno write setFNomeAluno;

    [FieldName('ENDERECO')]
    [PropriedadesCampo('ENDERECO', False, True, '', '', '', Inner)]
    property Endereco:Tstring read FEndereco write setFEndereco;

    [FieldName('TELEFONE')]
    [PropriedadesCampo('TELEFONE', False, True, '', '', '', Inner)]
    property Telefone:Tstring read FTelefone write setFTelefone;

    [FieldName('CPF')]
    [PropriedadesCampo('CPF', False, True, '', '', '', Inner)]
    property CPF:Tstring read FCPF write setFCPF;

    [FieldName('ID_PESSOA')]
    [PropriedadesCampo('ID_PESSOA', False, True, 'PESSOAS', 'ID_PESSOA', 'PES1', Inner)]
    property ID_PESSOA: TInteger read FID_PESSOA write setID_PESSOA;

    [PropriedadesCampo('ID_PESSOA', False, True, 'PESSOAS', 'ID_NOVO', 'PES1', Inner)]
    property ID_NOVO: TInteger read FID_NOVO write setID_NOVO;

    [PropriedadesCampo('ID_PESSOA_MAIS', False, True, 'PESSOAS', 'ID_PESSOA_MAIS', 'PES2', Inner)]
    property ID_PESSOA_MAIS: TInteger read FID_PESSOA_MAIS write setID_PESSOA_MAIS;


    function ToString: string; override;
end;

implementation

procedure TAluno.setFMatricula(value:Tstring);
begin
  FMatricula:= value;
end;

procedure TAluno.setFNomeAluno(value:Tstring);
begin
  FNomeAluno:= value;
end;

procedure TAluno.setFEndereco(value:Tstring);
begin
  FEndereco:= value;
end;

procedure TAluno.setFTelefone(value:Tstring);
begin
  FTelefone:= value;
end;

procedure TAluno.setID_NOVO(const Value: TInteger);
begin
  FID_NOVO := Value;
end;

procedure TAluno.setID_PESSOA(const Value: TInteger);
begin
  FID_PESSOA := Value;
end;

procedure TAluno.setID_PESSOA_MAIS(const Value: TInteger);
begin
  FID_PESSOA_MAIS := Value;
end;

procedure TAluno.setID_PRODUTO(const Value: TInteger);
begin
  FID_PRODUTO := Value;
end;

constructor TAluno.Create;
begin
 //
end;

procedure TAluno.setFCPF(value:Tstring);
begin
  FCPF:= value;
end;

function TAluno.toString;
begin
  Result := ' Matricula: '+ Matricula + ' Nome: ' + Nome + ' Endereco: ' + Endereco +
            ' Fone: ' + Telefone + ' CPF: ' + CPF;
end;

end.
