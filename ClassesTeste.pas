unit ClassesTeste;

interface

uses
  uAtribEntity, uGenericDAO, uTiposPrimitivos;

type
  [NomeTabela('PRODUTOS')]
  TProduto = class(TGenericDAO)
  private
    FID_PRODUTO: TInteger;
    FNOME: TString;
    FNOME_MARCA: TString;
    FID_MARCA: TInteger;
    procedure SetID_PRODUTO(const Value: TInteger);
    procedure SetNOME(const Value: TString);
    procedure SetID_MARCA(const Value: TInteger);
  public
    constructor Create; override;

    [DadosColuna('ID_PRODUTO', True, False)]
    property ID_PRODUTO: TInteger read FID_PRODUTO write SetID_PRODUTO;

    [DadosColuna('NOME', False, False)]
    property NOME: TString read FNOME write SetNOME;

    [DadosColuna('ID_MARCA', False, False)]
    [ChaveEstrangeira('MARCAS', 'MAR', 'ID_MARCA', Inner)]
    property ID_MARCA: TInteger read FID_MARCA write SetID_MARCA;

    [DadosColuna('NOME_MARCA', False, False)]
    [ChaveEstrangeira('MARCAS', 'MAR', '', Nenhum)]
    property NOME_MARCA: TString read FNOME_MARCA write FNome_Marca;
  end;

implementation

{ TProduto }

constructor TProduto.Create;
begin
  Classe := Self;
end;

procedure TProduto.SetID_MARCA(const Value: TInteger);
begin
  FID_MARCA := Value;
end;

procedure TProduto.SetID_PRODUTO(const Value: TInteger);
begin
  FID_PRODUTO := Value;
end;

procedure TProduto.SetNOME(const Value: TString);
begin
  FNOME := Value;
end;

end.
