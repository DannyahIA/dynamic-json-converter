unit Unit1;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  System.Rtti,
  System.JSON,
  System.SysUtils,
  System.TypInfo,
  System.Generics.Collections,
  Vcl.ComCtrls,
  DateUtils,

  // GUIA DTO
  GuiaMonitoramentoDto;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    tabParseJSON: TTabSheet;
    TabSheet2: TTabSheet;
    pnlParseJSON: TPanel;
    mmEntrada: TMemo;
    mmSaida: TMemo;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    btmParse: TButton;
    lblSaida: TLabel;
    Splitter3: TSplitter;
    lblEntrada: TLabel;
    pnlLabelParse: TPanel;
    Panel3: TPanel;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    Splitter6: TSplitter;
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    pnlLabelObj: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    procedure btObjToJsonClick(Sender: TObject);
    procedure btmParseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  // CLASSES EXEMPLO
type
  TProduto = class(TObject)
  private
    FNome: string;
    FPreco: double;
    FProdutos: TArray<TProduto>;
  public
    property Nome: string read FNome write FNome;
    property Preco: double read FPreco write FPreco;
    property Produtos: TArray<TProduto> read FProdutos write FProdutos;
  end;

  TLoja = class
  private
    FNome: string;
    FProdutos: TArray<TProduto>;
  public
    property Nome: string read FNome write FNome;
    property Produtos: TArray<TProduto> read FProdutos write FProdutos;
  end;

  // CLASSES EXEMPLO 2
type
  TCarro = class
  private
    FNome: string;
    FModelo: string;
  public
    property Nome: string read FNome write FNome;
    property Modelo: string read FModelo write FModelo;
  end;

  TMarca = class
  private
    FNome: string;
    FCarros: TArray<TCarro>;
  public
    property Nome: string read FNome write FNome;
    property Carros: TArray<TCarro> read FCarros write FCarros;
  end;

  // OTHER
  TSomeClass = class
    procedure JSONToObject(AObject: TObject; AJSON: TJSONObject);
    function ObjectToJSON(AObject: TObject): TJSONObject;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// OBJECT TO JSON
procedure TForm1.btmParseClick(Sender: TObject);
var
  Loja: TLoja;
  // GuiaMonitoramento: TGuiaMonitoramento;

  JSONObj: TJSONObject;
begin
  // Parse da string para um JsonObject
  JSONObj := TJSONObject.ParseJSONValue(mmEntrada.Text) as TJSONObject;
  try
    Loja := TLoja.Create;
    // GuiaMonitoramento := TGuiaMonitoramento.Create;
    try
      TSomeClass.Create.JSONToObject(Loja, JSONObj);
      // TSomeClass.Create.JSONToObject(GuiaMonitoramento, JSONObj);

      mmSaida.Clear;

      // Agora o objeto deve estar populado com os dados do JSON
      mmSaida.Lines.Add('Nome da Loja: ' + Loja.Nome);
      mmSaida.Lines.Add('Produtos:');
      for var Produto in Loja.Produtos do
      begin
        mmSaida.Lines.Add('    Nome do Produto: ' + Produto.Nome);
        mmSaida.Lines.Add('    Pre�o do Produto: ' + FloatToStr(Produto.Preco));
        mmSaida.Lines.Add(SLineBreak);
      end;

      // mmSaida.Lines.Add('Guia: ');
      //
      // mmSaida.Lines.Add('GuiaId: ' + GuiaMonitoramento.GuiaId);
      // mmSaida.Lines.Add('RegistroAns: ' + GuiaMonitoramento.RegistroAns);
      // mmSaida.Lines.Add('TipoRegistroId: ' + GuiaMonitoramento.TipoRegistroId);
      // mmSaida.Lines.Add('VersaoTissPrestador: ' +
      // GuiaMonitoramento.VersaoTissPrestador);
      // mmSaida.Lines.Add('FormaEnvioId: ' + GuiaMonitoramento.FormaEnvioId);
      // mmSaida.Lines.Add('PrestadorCnesId: ' +
      // GuiaMonitoramento.PrestadorCnesId);
      // mmSaida.Lines.Add('PrestadorIndicadorIdentificacaoId: ' +
      // GuiaMonitoramento.PrestadorIndicadorIdentificacaoId);
      // mmSaida.Lines.Add('PrestadorCnpjCpf: ' +
      // GuiaMonitoramento.PrestadorCnpjCpf);
      // mmSaida.Lines.Add('PrestadorMunicipioId: ' +
      // GuiaMonitoramento.PrestadorMunicipioId);
      // mmSaida.Lines.Add('BeneficiarioCns: ' +
      // GuiaMonitoramento.BeneficiarioCns);
      // mmSaida.Lines.Add('BeneficiarioCpf: ' +
      // GuiaMonitoramento.BeneficiarioCpf);
      // mmSaida.Lines.Add('BeneficiarioSexoId: ' +
      // GuiaMonitoramento.BeneficiarioSexoId);
      // mmSaida.Lines.Add('BeneficiarioDataNascimento: ' +
      // DateToStr(GuiaMonitoramento.BeneficiarioDataNascimento));
      // mmSaida.Lines.Add('BeneficiarioMunicipioId: ' +
      // GuiaMonitoramento.BeneficiarioMunicipioId);
      // mmSaida.Lines.Add('BeneficiarioNumeroRegistroPlano: ' +
      // GuiaMonitoramento.BeneficiarioNumeroRegistroPlano);
      // mmSaida.Lines.Add('TipoGuiaId: ' + GuiaMonitoramento.TipoGuiaId);
      // mmSaida.Lines.Add('OrigemGuiaId: ' + GuiaMonitoramento.OrigemGuiaId);
      // mmSaida.Lines.Add('Competencia: ' + GuiaMonitoramento.Competencia);
      // mmSaida.Lines.Add('NumeroGuiaPrestador: ' +
      // GuiaMonitoramento.NumeroGuiaPrestador);
      // mmSaida.Lines.Add('NumeroGuiaOperadora: ' +
      // GuiaMonitoramento.NumeroGuiaOperadora);
      // mmSaida.Lines.Add('IdentificadorReembolso: ' +
      // GuiaMonitoramento.IdentificadorReembolso);
      // mmSaida.Lines.Add('IdentificacaoValorPreestabelecido: ' +
      // GuiaMonitoramento.IdentificacaoValorPreestabelecido);
      // mmSaida.Lines.Add('GuiaSolicitacaoInternacao: ' +
      // GuiaMonitoramento.GuiaSolicitacaoInternacao);
      // mmSaida.Lines.Add('DataSolicitacao: ' +
      // DateToStr(GuiaMonitoramento.DataSolicitacao));
      // mmSaida.Lines.Add('NumeroGuiaSpsadtPrincipal: ' +
      // GuiaMonitoramento.NumeroGuiaSpsadtPrincipal);
      // mmSaida.Lines.Add('DataAutorizacao: ' +
      // DateToStr(GuiaMonitoramento.DataAutorizacao));
      // mmSaida.Lines.Add('DataRealizacao: ' +
      // DateToStr(GuiaMonitoramento.DataRealizacao));
      // mmSaida.Lines.Add('DataProtocoloCobranca: ' +
      // DateToStr(GuiaMonitoramento.DataProtocoloCobranca));
      // mmSaida.Lines.Add('DataPagamento: ' +
      // DateToStr(GuiaMonitoramento.DataPagamento));
      // mmSaida.Lines.Add('DataProcessamentoGuia: ' +
      // DateToStr(GuiaMonitoramento.DataProcessamentoGuia));
      // mmSaida.Lines.Add('DataFimPeriodo: ' +
      // DateToStr(GuiaMonitoramento.DataFimPeriodo));
      // mmSaida.Lines.Add('DataInicialFaturamento: ' +
      // DateToStr(GuiaMonitoramento.DataInicialFaturamento));
      // mmSaida.Lines.Add('ValorTotalInformado: ' +
      // FloatToStr(GuiaMonitoramento.ValorTotalInformado));
      // mmSaida.Lines.Add('ValorProcessado: ' +
      // FloatToStr(GuiaMonitoramento.ValorProcessado));
      // mmSaida.Lines.Add('ValorTotalPagoProcedimento: ' +
      // FloatToStr(GuiaMonitoramento.ValorTotalPagoProcedimento));
      // mmSaida.Lines.Add('ValorTotalDiaria: ' +
      // FloatToStr(GuiaMonitoramento.ValorTotalDiaria));
      // mmSaida.Lines.Add('ValorTotalTaxa: ' +
      // FloatToStr(GuiaMonitoramento.ValorTotalTaxa));
      // mmSaida.Lines.Add('ValorTotalMaterial: ' +
      // FloatToStr(GuiaMonitoramento.ValorTotalMaterial));
      // mmSaida.Lines.Add('ValorTotalOpme: ' +
      // FloatToStr(GuiaMonitoramento.ValorTotalOpme));
      // mmSaida.Lines.Add('ValorTotalMedicamento: ' +
      // FloatToStr(GuiaMonitoramento.ValorTotalMedicamento));
      // mmSaida.Lines.Add('ValorGlosaGuia: ' +
      // FloatToStr(GuiaMonitoramento.ValorGlosaGuia));
      // mmSaida.Lines.Add('ValorPagoGuia: ' +
      // FloatToStr(GuiaMonitoramento.ValorPagoGuia));
      // mmSaida.Lines.Add('ValorPagoFornecedor: ' +
      // FloatToStr(GuiaMonitoramento.ValorPagoFornecedor));
      // mmSaida.Lines.Add('ValorTotalPagoTabelaPropria: ' +
      // FloatToStr(GuiaMonitoramento.ValorTotalPagoTabelaPropria));
      // mmSaida.Lines.Add('ValorTotalPagoCoparticipacao: ' +
      // FloatToStr(GuiaMonitoramento.ValorTotalPagoCoparticipacao));
      // mmSaida.Lines.Add('IndicacaoRecemNato: ' +
      // GuiaMonitoramento.IndicacaoRecemNato);
      // mmSaida.Lines.Add('IndicacaoAcidente: ' +
      // GuiaMonitoramento.IndicacaoAcidente);
      // mmSaida.Lines.Add('CaraterAtendimento: ' +
      // GuiaMonitoramento.CaraterAtendimento);
      // mmSaida.Lines.Add('TipoInternacao: ' + GuiaMonitoramento.TipoInternacao);
      // mmSaida.Lines.Add('RegimeInternacao: ' +
      // GuiaMonitoramento.RegimeInternacao);
      // mmSaida.Lines.Add('SaudeOcupacional: ' +
      // GuiaMonitoramento.SaudeOcupacional);
      // mmSaida.Lines.Add('TipoFaturamento: ' +
      // GuiaMonitoramento.TipoFaturamento);
      // mmSaida.Lines.Add('DiariaAcompanhante: ' +
      // IntToStr(GuiaMonitoramento.DiariaAcompanhante));
      // mmSaida.Lines.Add('DiariaUti: ' + IntToStr(GuiaMonitoramento.DiariaUti));
      // mmSaida.Lines.Add('MotivoSaidaId: ' + GuiaMonitoramento.MotivoSaidaId);
      // mmSaida.Lines.Add('CboExecutante: ' + GuiaMonitoramento.CboExecutante);
      // mmSaida.Lines.Add('TipoConsulta: ' + GuiaMonitoramento.TipoConsulta);
      // mmSaida.Lines.Add('RegistroAnsOperadoraIntermediaria: ' +
      // GuiaMonitoramento.RegistroAnsOperadoraIntermediaria);
      // mmSaida.Lines.Add('TipoAtendimentoIntermediarioId: ' +
      // GuiaMonitoramento.TipoAtendimentoIntermediarioId);
      // mmSaida.Lines.Add('TipoAtendimentoId: ' +
      // GuiaMonitoramento.TipoAtendimentoId);
      // mmSaida.Lines.Add('RegimeAtendimentoId: ' +
      // GuiaMonitoramento.RegimeAtendimentoId);
      // mmSaida.Lines.Add('FormasRemuneracaoId: ' +
      // GuiaMonitoramento.FormasRemuneracaoId);
      // mmSaida.Lines.Add('ValorRemuneracaoId: ' +
      // FloatToStr(GuiaMonitoramento.ValorRemuneracaoId));
      // mmSaida.Lines.Add('StatusAnsId: ' + GuiaMonitoramento.StatusAnsId);
      // mmSaida.Lines.Add('GuiaPendente: ' + GuiaMonitoramento.GuiaPendente);
      // mmSaida.Lines.Add('StatusEnviaAns: ' + GuiaMonitoramento.StatusEnviaAns);
      // mmSaida.Lines.Add('ChaveAcesso: ' + GuiaMonitoramento.ChaveAcesso);
      // mmSaida.Lines.Add(SLineBreak);
      //
      // for var FormaRemuneracao in GuiaMonitoramento.FormasRemuneracao do
      // begin
      // mmSaida.Lines.Add('Forma Remunera��o: ');
      //
      // mmSaida.Lines.Add(' Guia Forma Remunera��o Id: ' +
      // FormaRemuneracao.GuiaFormaRemuneracaoId);
      // mmSaida.Lines.Add(' Guia Id: ' + FormaRemuneracao.GuiaId);
      // mmSaida.Lines.Add(' Forma Remunera��o Id: ' +
      // FormaRemuneracao.FormaRemuneracaoId);
      // mmSaida.Lines.Add(' Valor Remunera��o: ' +
      // FloatToStr(FormaRemuneracao.ValorRemuneracao));
      //
      // mmSaida.Lines.Add(SLineBreak);
      // end;
      //
      // for var DiagnosticoCid10 in GuiaMonitoramento.DiagnosticosCid10 do
      // begin
      // mmSaida.Lines.Add('Diagn�stico CID-10: ');
      //
      // mmSaida.Lines.Add(' CID-10 Id: ' + DiagnosticoCid10.Cid10Id);
      // mmSaida.Lines.Add(' Sequ�ncia: ' + IntToStr(DiagnosticoCid10.Sequencia));
      //
      // mmSaida.Lines.Add(SLineBreak);
      // end;
      //
      // for var DeclaracaoNascido in GuiaMonitoramento.DeclaracoesNascido do
      // begin
      // mmSaida.Lines.Add('Declara��o Nascido: ');
      //
      // mmSaida.Lines.Add(' Declara��o Nascido Id: ' +
      // DeclaracaoNascido.DeclaracaoNascidoId);
      // mmSaida.Lines.Add(' Guia Id: ' + DeclaracaoNascido.GuiaId);
      // mmSaida.Lines.Add(' Sequ�ncia: ' +
      // IntToStr(DeclaracaoNascido.Sequencia));
      // mmSaida.Lines.Add(' Numero Declara��o: ' +
      // DeclaracaoNascido.NumeroDeclaracao);
      //
      // mmSaida.Lines.Add(SLineBreak);
      // end;
      //
      // for var DeclaracaoObito in GuiaMonitoramento.DeclaracoesObito do
      // begin
      // mmSaida.Lines.Add('Declara��o �bito: ');
      //
      // mmSaida.Lines.Add(' Declara��o �bito Id: ' +
      // DeclaracaoObito.DeclaracaoObitoId);
      // mmSaida.Lines.Add(' Guia Id: ' + DeclaracaoObito.GuiaId);
      // mmSaida.Lines.Add(' Sequ�ncia: ' + IntToStr(DeclaracaoObito.Sequencia));
      // mmSaida.Lines.Add(' Numero Declara��o: ' +
      // DeclaracaoObito.NumeroDeclaracao);
      //
      // mmSaida.Lines.Add(SLineBreak);
      // end;
      //
      // for var Procedimento in GuiaMonitoramento.Procedimentos do
      // begin
      // mmSaida.Lines.Add('Procedimento: ');
      //
      // mmSaida.Lines.Add(' Procedimento Id: ' + Procedimento.ProcedimentoId);
      // mmSaida.Lines.Add(' Guia Id: ' + Procedimento.GuiaId);
      // mmSaida.Lines.Add(' Sequ�ncia: ' + IntToStr(Procedimento.Sequencia));
      // mmSaida.Lines.Add(' C�digo Tabela Monitor Id: ' +
      // Procedimento.CodigoTabelaMonitorId);
      // mmSaida.Lines.Add(' Grupo Procedimento Id: ' +
      // Procedimento.GrupoProcedimentoId);
      // mmSaida.Lines.Add(' C�digo Procedimento Id: ' +
      // Procedimento.CodigoProcedimentoId);
      // mmSaida.Lines.Add(' Dente Id: ' + Procedimento.DenteId);
      // mmSaida.Lines.Add(' Regi�o Boca Id: ' + Procedimento.RegiaoBocaId);
      // mmSaida.Lines.Add(' Dente Face Id: ' + Procedimento.DenteFaceId);
      // mmSaida.Lines.Add(' Quantidade Informada: ' +
      // FloatToStr(Procedimento.QuantidadeInformada));
      // mmSaida.Lines.Add(' Valor Informado: ' +
      // FloatToStr(Procedimento.ValorInformado));
      // mmSaida.Lines.Add(' Quantidade Paga: ' +
      // FloatToStr(Procedimento.QuantidadePaga));
      // mmSaida.Lines.Add(' Unidade Medida Id: ' + Procedimento.UnidadeMedidaId);
      // mmSaida.Lines.Add(' Valor Pago Procedimento: ' +
      // FloatToStr(Procedimento.ValorPagoProcedimento));
      // mmSaida.Lines.Add(' Valor Pago Fornecedor: ' +
      // FloatToStr(Procedimento.ValorPagoFornecedor));
      // mmSaida.Lines.Add(' Cnpj Fornecedor: ' + Procedimento.CnpjFornecedor);
      // mmSaida.Lines.Add(' Valor Coparticipa��o: ' +
      // FloatToStr(Procedimento.ValorCoparticipacao));
      //
      // mmSaida.Lines.Add(SLineBreak);
      //
      // for var ItemPacote in Procedimento.ItemsPacote do
      // begin
      // mmSaida.Lines.Add('Item Pacote: ');
      //
      // mmSaida.Lines.Add(' Item Pacote Id: ' + ItemPacote.ItemPacoteId);
      // mmSaida.Lines.Add(' Guia Procedimento Id: ' +
      // ItemPacote.GuiaProcedimentoId);
      // mmSaida.Lines.Add(' C�digo Tabela Pacote Id: ' +
      // ItemPacote.CodigoTabelaPacoteId);
      // mmSaida.Lines.Add(' C�digo Procedimento: ' +
      // ItemPacote.CodigoProcedimento);
      // mmSaida.Lines.Add(' Quantidade: ' + FloatToStr(ItemPacote.Quantidade));
      //
      // mmSaida.Lines.Add(SLineBreak);
      // end;
      // end;

    finally
      Loja.Free;
      // GuiaMonitoramento.Free;
    end;
  finally
    JSONObj.Free;
  end;
end;

procedure TSomeClass.JSONToObject(AObject: TObject; AJSON: TJSONObject);
var
  Context: TRttiContext;        // Contexto RTTI para acessar as propriedades do objeto
  RttiType: TRttiType;          // Representa o tipo do objeto usando RTTI
  Prop: TRttiProperty;          // Propriedade RTTI para cada campo no objeto
  JsonPair: TJSONPair;          // Representa um par de chave-valor no JSON
  JsonArray: TJSONArray;        // Representa um array no JSON
  ElemClass: TClass;            // Classe dos elementos no array din�mico
  I: integer;                   // Iterador de loop
  SubObject: TObject;           // Objeto auxiliar para elementos de arrays
  ArrayValue: TValue;           // Valor do array final para ser atribu�do � propriedade
  TValueArray: TArray<TValue>;  // Array de valores para os elementos do array din�mico
begin
  Context := TRttiContext.Create; // Inicializa o contexto RTTI
  try
    RttiType := Context.GetType(AObject.ClassType); // Obt�m o tipo RTTI do objeto

    // Itera sobre cada par chave-valor do JSON
    for JsonPair in AJSON do
    begin
      // Tenta obter a propriedade do objeto correspondente � chave do JSON
      Prop := RttiType.GetProperty(JsonPair.JsonString.Value);

      // Verifica se a propriedade existe no objeto
      if Assigned(Prop) then
      begin
        // Verifica se a propriedade � um array din�mico e se o valor no JSON � um array
        if (Prop.PropertyType.TypeKind = tkDynArray) and
          (JsonPair.JsonValue is TJSONArray) then
        begin
          JsonArray := TJSONArray(JsonPair.JsonValue); // Converte o valor JSON para TJSONArray
          SetLength(TValueArray, JsonArray.Count); // Define o tamanho do array com base na quantidade de itens do JSON

          // Obt�m a classe dos elementos do array din�mico
          ElemClass := (Prop.PropertyType as TRttiDynamicArrayType)
            .ElementType.AsInstance.MetaclassType;

          // Itera sobre os itens do array no JSON
          for I := 0 to JsonArray.Count - 1 do
          begin
            // Se o item do array no JSON for um objeto, processa recursivamente
            if JsonArray.Items[I] is TJSONObject then
            begin
              SubObject := ElemClass.Create; // Cria uma inst�ncia do tipo de elemento do array
              try
                // Chama recursivamente para preencher o subobjeto com o JSON
                JSONToObject(SubObject, JsonArray.Items[I] as TJSONObject);
                TValueArray[I] := TValue.From(SubObject); // Armazena o subobjeto no array
              except
                SubObject.Free; // Libera o subobjeto em caso de erro
                raise;
              end;
            end
            else
              raise Exception.CreateFmt
                ('Array item at index %d is not a valid JSON object.', [I]); // Erro se o item do array n�o for um objeto JSON
          end;

          // Converte o array de valores para um TValue e o atribui � propriedade do objeto
          ArrayValue := TValue.FromArray(Prop.PropertyType.Handle, TValueArray);
          Prop.SetValue(AObject, ArrayValue);
        end
        else
        begin
          // Lida com propriedades de tipos simples (strings, n�meros, etc.)

          // Verifica se o valor do campo no JSON � null
          if JsonPair.JsonValue.Null then
            Prop.SetValue(AObject, TValue.Empty) // Atribui um valor vazio
          else if JsonPair.JsonValue is TJSONNumber then
          begin
            // Verifica se a propriedade � do tipo inteiro
            if Prop.PropertyType.Handle = TypeInfo(integer) then
              Prop.SetValue(AObject,
                TValue.From<integer>(TJSONNumber(JsonPair.JsonValue).AsInt))
            // Verifica se a propriedade � do tipo double (decimal)
            else if Prop.PropertyType.Handle = TypeInfo(double) then
              Prop.SetValue(AObject,
                TValue.From<double>(TJSONNumber(JsonPair.JsonValue).AsDouble))
            else
              raise Exception.CreateFmt
                ('Tipo num�rico n�o suportado para a propriedade "%s".', [Prop.Name]);
          end
          else if JsonPair.JsonValue is TJSONTrue then
            Prop.SetValue(AObject, TValue.From<Boolean>(True)) // Atribui valor booleano True
          else if JsonPair.JsonValue is TJSONFalse then
            Prop.SetValue(AObject, TValue.From<Boolean>(False)) // Atribui valor booleano False
          else if JsonPair.JsonValue is TJSONString then
          begin
            // Verifica se a propriedade � uma data
            if Prop.PropertyType.Handle = TypeInfo(TDate) then
              Prop.SetValue(AObject,
                TValue.From<TDate>(ISO8601ToDate(TJSONString(JsonPair.JsonValue)
                .Value, False)))
            // Verifica se a propriedade � um DateTime
            else if Prop.PropertyType.Handle = TypeInfo(TDateTime) then
              Prop.SetValue(AObject,
                TValue.From<TDateTime>
                (ISO8601ToDate(TJSONString(JsonPair.JsonValue).Value, True)))
            else
              // Atribui o valor como string
              Prop.SetValue(AObject,
                TValue.From<string>(TJSONString(JsonPair.JsonValue).Value));
          end
          else
            raise Exception.CreateFmt
              ('Tipo de JSON n�o suportado para a propriedade "%s".', [Prop.Name]); // Erro se o tipo JSON n�o for suportado
        end;
      end;
    end;
  finally
    Context.Free; // Libera o contexto RTTI ao final
  end;
end;

procedure TForm1.btObjToJsonClick(Sender: TObject);
var
  Loja: TLoja;
  JSON: TJSONObject;
begin
  // Loja := TLoja.Create;
  // try
  // // Popule Loja aqui
  // JSON := ObjectToJSON(Loja);
  // try
  // Writeln(JSON.ToString);
  // finally
  // JSON.Free;
  // end;
  // finally
  // Loja.Free;
  // end;
end;

function TSomeClass.ObjectToJSON(AObject: TObject): TJSONObject;
begin
end;

end.
