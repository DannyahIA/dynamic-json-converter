unit Unit1;

interface

uses
  Vcl.Forms,
  System.JSON,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,

  // DTOs de exemplo
  DtosExemplo,

  // FUNÇÃO PARA CONVERTER JSON
  JSONDynamicConverter;

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
    pnlHeader: TPanel;
    pnlObjectToJSON: TPanel;
    Splitter4: TSplitter;
    Splitter5: TSplitter;
    Splitter6: TSplitter;
    mmEntrada3: TMemo;
    mmSaida3: TMemo;
    btmConvert: TButton;
    pnlHeader3: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    tabParseJSONArray: TTabSheet;
    pnlParseJSONArray: TPanel;
    Splitter7: TSplitter;
    Splitter8: TSplitter;
    Splitter9: TSplitter;
    mmEntrada2: TMemo;
    mmSaida2: TMemo;
    btmParseJSONArray: TButton;
    pnlHeader2: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    tabObjectListToJSONArray: TTabSheet;
    pnlObjectListToJSONArray: TPanel;
    Splitter10: TSplitter;
    Splitter11: TSplitter;
    Splitter12: TSplitter;
    mmEntrada4: TMemo;
    mmSaida4: TMemo;
    btmObjectListToJSONArray: TButton;
    pnlHeader4: TPanel;
    Label5: TLabel;
    Label6: TLabel;
    procedure btObjToJsonClick(Sender: TObject);
    procedure btmParseClick(Sender: TObject);
    procedure btmParseJSONArrayClick(Sender: TObject);
    procedure btmObjectListToJSONArrayClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

// JSON PARA OBJETO
procedure TForm1.btmParseClick(Sender: TObject);
var
  GuiaMonitoramento: TGuiaMonitoramento;
  JSONConverter: TJSONDynConverter;
  JSONObj: TJSONObject;
begin
  // Parse da string para um JsonObject
  JSONObj := TJSONObject.ParseJSONValue(mmEntrada.Text) as TJSONObject;
  try
    GuiaMonitoramento := TGuiaMonitoramento.Create;
    try
      JSONConverter := TJSONDynConverter.Create;
      JSONConverter.Create.JSONToObject(JSONObj, GuiaMonitoramento);
      JSONConverter.Free;

      // Limpa a saída antes de colocar novos dados
      mmSaida.Clear;

      // Popula o objeto com os dados do json
      mmSaida.Lines.Add('Guia: ');
      mmSaida.Lines.Add('GuiaId: ' + GuiaMonitoramento.GuiaId);
      mmSaida.Lines.Add('RegistroAns: ' + GuiaMonitoramento.RegistroAns);
      mmSaida.Lines.Add('TipoRegistroId: ' + GuiaMonitoramento.TipoRegistroId);
      mmSaida.Lines.Add('VersaoTissPrestador: ' +
        GuiaMonitoramento.VersaoTissPrestador);
      mmSaida.Lines.Add('FormaEnvioId: ' + GuiaMonitoramento.FormaEnvioId);
      mmSaida.Lines.Add('PrestadorCnesId: ' +
        GuiaMonitoramento.PrestadorCnesId);
      mmSaida.Lines.Add('PrestadorIndicadorIdentificacaoId: ' +
        GuiaMonitoramento.PrestadorIndicadorIdentificacaoId);
      mmSaida.Lines.Add('PrestadorCnpjCpf: ' +
        GuiaMonitoramento.PrestadorCnpjCpf);
      mmSaida.Lines.Add('PrestadorMunicipioId: ' +
        GuiaMonitoramento.PrestadorMunicipioId);
      mmSaida.Lines.Add('BeneficiarioCns: ' +
        GuiaMonitoramento.BeneficiarioCns);
      mmSaida.Lines.Add('BeneficiarioCpf: ' +
        GuiaMonitoramento.BeneficiarioCpf);
      mmSaida.Lines.Add('BeneficiarioSexoId: ' +
        GuiaMonitoramento.BeneficiarioSexoId);
      mmSaida.Lines.Add('BeneficiarioDataNascimento: ' +
        DateToStr(GuiaMonitoramento.BeneficiarioDataNascimento));
      mmSaida.Lines.Add('BeneficiarioMunicipioId: ' +
        GuiaMonitoramento.BeneficiarioMunicipioId);
      mmSaida.Lines.Add('BeneficiarioNumeroRegistroPlano: ' +
        GuiaMonitoramento.BeneficiarioNumeroRegistroPlano);
      mmSaida.Lines.Add('TipoGuiaId: ' + GuiaMonitoramento.TipoGuiaId);
      mmSaida.Lines.Add('OrigemGuiaId: ' + GuiaMonitoramento.OrigemGuiaId);
      mmSaida.Lines.Add('Competencia: ' + GuiaMonitoramento.Competencia);
      mmSaida.Lines.Add('NumeroGuiaPrestador: ' +
        GuiaMonitoramento.NumeroGuiaPrestador);
      mmSaida.Lines.Add('NumeroGuiaOperadora: ' +
        GuiaMonitoramento.NumeroGuiaOperadora);
      mmSaida.Lines.Add('IdentificadorReembolso: ' +
        GuiaMonitoramento.IdentificadorReembolso);
      mmSaida.Lines.Add('IdentificacaoValorPreestabelecido: ' +
        GuiaMonitoramento.IdentificacaoValorPreestabelecido);
      mmSaida.Lines.Add('GuiaSolicitacaoInternacao: ' +
        GuiaMonitoramento.GuiaSolicitacaoInternacao);
      mmSaida.Lines.Add('DataSolicitacao: ' +
        DateToStr(GuiaMonitoramento.DataSolicitacao));
      mmSaida.Lines.Add('NumeroGuiaSpsadtPrincipal: ' +
        GuiaMonitoramento.NumeroGuiaSpsadtPrincipal);
      mmSaida.Lines.Add('DataAutorizacao: ' +
        DateToStr(GuiaMonitoramento.DataAutorizacao));
      mmSaida.Lines.Add('DataRealizacao: ' +
        DateToStr(GuiaMonitoramento.DataRealizacao));
      mmSaida.Lines.Add('DataProtocoloCobranca: ' +
        DateToStr(GuiaMonitoramento.DataProtocoloCobranca));
      mmSaida.Lines.Add('DataPagamento: ' +
        DateToStr(GuiaMonitoramento.DataPagamento));
      mmSaida.Lines.Add('DataProcessamentoGuia: ' +
        DateToStr(GuiaMonitoramento.DataProcessamentoGuia));
      mmSaida.Lines.Add('DataFimPeriodo: ' +
        DateToStr(GuiaMonitoramento.DataFimPeriodo));
      mmSaida.Lines.Add('DataInicialFaturamento: ' +
        DateToStr(GuiaMonitoramento.DataInicialFaturamento));
      mmSaida.Lines.Add('ValorTotalInformado: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalInformado));
      mmSaida.Lines.Add('ValorProcessado: ' +
        FloatToStr(GuiaMonitoramento.ValorProcessado));
      mmSaida.Lines.Add('ValorTotalPagoProcedimento: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalPagoProcedimento));
      mmSaida.Lines.Add('ValorTotalDiaria: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalDiaria));
      mmSaida.Lines.Add('ValorTotalTaxa: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalTaxa));
      mmSaida.Lines.Add('ValorTotalMaterial: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalMaterial));
      mmSaida.Lines.Add('ValorTotalOpme: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalOpme));
      mmSaida.Lines.Add('ValorTotalMedicamento: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalMedicamento));
      mmSaida.Lines.Add('ValorGlosaGuia: ' +
        FloatToStr(GuiaMonitoramento.ValorGlosaGuia));
      mmSaida.Lines.Add('ValorPagoGuia: ' +
        FloatToStr(GuiaMonitoramento.ValorPagoGuia));
      mmSaida.Lines.Add('ValorPagoFornecedor: ' +
        FloatToStr(GuiaMonitoramento.ValorPagoFornecedor));
      mmSaida.Lines.Add('ValorTotalPagoTabelaPropria: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalPagoTabelaPropria));
      mmSaida.Lines.Add('ValorTotalPagoCoparticipacao: ' +
        FloatToStr(GuiaMonitoramento.ValorTotalPagoCoparticipacao));
      mmSaida.Lines.Add('IndicacaoRecemNato: ' +
        GuiaMonitoramento.IndicacaoRecemNato);
      mmSaida.Lines.Add('IndicacaoAcidente: ' +
        GuiaMonitoramento.IndicacaoAcidente);
      mmSaida.Lines.Add('CaraterAtendimento: ' +
        GuiaMonitoramento.CaraterAtendimento);
      mmSaida.Lines.Add('TipoInternacao: ' + GuiaMonitoramento.TipoInternacao);
      mmSaida.Lines.Add('RegimeInternacao: ' +
        GuiaMonitoramento.RegimeInternacao);
      mmSaida.Lines.Add('SaudeOcupacional: ' +
        GuiaMonitoramento.SaudeOcupacional);
      mmSaida.Lines.Add('TipoFaturamento: ' +
        GuiaMonitoramento.TipoFaturamento);
      mmSaida.Lines.Add('DiariaAcompanhante: ' +
        IntToStr(GuiaMonitoramento.DiariaAcompanhante));
      mmSaida.Lines.Add('DiariaUti: ' + IntToStr(GuiaMonitoramento.DiariaUti));
      mmSaida.Lines.Add('MotivoSaidaId: ' + GuiaMonitoramento.MotivoSaidaId);
      mmSaida.Lines.Add('CboExecutante: ' + GuiaMonitoramento.CboExecutante);
      mmSaida.Lines.Add('TipoConsulta: ' + GuiaMonitoramento.TipoConsulta);
      mmSaida.Lines.Add('RegistroAnsOperadoraIntermediaria: ' +
        GuiaMonitoramento.RegistroAnsOperadoraIntermediaria);
      mmSaida.Lines.Add('TipoAtendimentoIntermediarioId: ' +
        GuiaMonitoramento.TipoAtendimentoIntermediarioId);
      mmSaida.Lines.Add('TipoAtendimentoId: ' +
        GuiaMonitoramento.TipoAtendimentoId);
      mmSaida.Lines.Add('RegimeAtendimentoId: ' +
        GuiaMonitoramento.RegimeAtendimentoId);
      mmSaida.Lines.Add('FormasRemuneracaoId: ' +
        GuiaMonitoramento.FormasRemuneracaoId);
      mmSaida.Lines.Add('ValorRemuneracaoId: ' +
        FloatToStr(GuiaMonitoramento.ValorRemuneracaoId));
      mmSaida.Lines.Add('StatusAnsId: ' + GuiaMonitoramento.StatusAnsId);
      mmSaida.Lines.Add('GuiaPendente: ' + GuiaMonitoramento.GuiaPendente);
      mmSaida.Lines.Add('StatusEnviaAns: ' + GuiaMonitoramento.StatusEnviaAns);
      mmSaida.Lines.Add('ChaveAcesso: ' + GuiaMonitoramento.ChaveAcesso);
      mmSaida.Lines.Add(SLineBreak);

      for var FormaRemuneracao in GuiaMonitoramento.FormasRemuneracao do
      begin
        mmSaida.Lines.Add('Forma Remuneração: ');

        mmSaida.Lines.Add(' Guia Forma Remuneração Id: ' +
          FormaRemuneracao.GuiaFormaRemuneracaoId);
        mmSaida.Lines.Add(' Guia Id: ' + FormaRemuneracao.GuiaId);
        mmSaida.Lines.Add(' Forma Remuneração Id: ' +
          FormaRemuneracao.FormaRemuneracaoId);
        mmSaida.Lines.Add(' Valor Remuneração: ' +
          FloatToStr(FormaRemuneracao.ValorRemuneracao));

        mmSaida.Lines.Add(SLineBreak);
      end;

      for var DiagnosticoCid10 in GuiaMonitoramento.DiagnosticosCid10 do
      begin
        mmSaida.Lines.Add('Diagnóstico CID-10: ');

        mmSaida.Lines.Add(' CID-10 Id: ' + DiagnosticoCid10.Cid10Id);
        mmSaida.Lines.Add(' Sequência: ' +
          IntToStr(DiagnosticoCid10.Sequencia));

        mmSaida.Lines.Add(SLineBreak);
      end;

      for var DeclaracaoNascido in GuiaMonitoramento.DeclaracoesNascido do
      begin
        mmSaida.Lines.Add('Declaração Nascido: ');

        mmSaida.Lines.Add(' Declaração Nascido Id: ' +
          DeclaracaoNascido.DeclaracaoNascidoId);
        mmSaida.Lines.Add(' Guia Id: ' + DeclaracaoNascido.GuiaId);
        mmSaida.Lines.Add(' Sequência: ' +
          IntToStr(DeclaracaoNascido.Sequencia));
        mmSaida.Lines.Add(' Numero Declaração: ' +
          DeclaracaoNascido.NumeroDeclaracao);

        mmSaida.Lines.Add(SLineBreak);
      end;

      for var DeclaracaoObito in GuiaMonitoramento.DeclaracoesObito do
      begin
        mmSaida.Lines.Add('Declaração Óbito: ');

        mmSaida.Lines.Add(' Declaração Óbito Id: ' +
          DeclaracaoObito.DeclaracaoObitoId);
        mmSaida.Lines.Add(' Guia Id: ' + DeclaracaoObito.GuiaId);
        mmSaida.Lines.Add(' Sequência: ' + IntToStr(DeclaracaoObito.Sequencia));
        mmSaida.Lines.Add(' Numero Declaração: ' +
          DeclaracaoObito.NumeroDeclaracao);

        mmSaida.Lines.Add(SLineBreak);
      end;

      for var Procedimento in GuiaMonitoramento.Procedimentos do
      begin
        mmSaida.Lines.Add('Procedimento: ');

        mmSaida.Lines.Add(' Procedimento Id: ' + Procedimento.ProcedimentoId);
        mmSaida.Lines.Add(' Guia Id: ' + Procedimento.GuiaId);
        mmSaida.Lines.Add(' Sequência: ' + IntToStr(Procedimento.Sequencia));
        mmSaida.Lines.Add(' Código Tabela Monitor Id: ' +
          Procedimento.CodigoTabelaMonitorId);
        mmSaida.Lines.Add(' Grupo Procedimento Id: ' +
          Procedimento.GrupoProcedimentoId);
        mmSaida.Lines.Add(' Código Procedimento Id: ' +
          Procedimento.CodigoProcedimentoId);
        mmSaida.Lines.Add(' Dente Id: ' + Procedimento.DenteId);
        mmSaida.Lines.Add(' Região Boca Id: ' + Procedimento.RegiaoBocaId);
        mmSaida.Lines.Add(' Dente Face Id: ' + Procedimento.DenteFaceId);
        mmSaida.Lines.Add(' Quantidade Informada: ' +
          FloatToStr(Procedimento.QuantidadeInformada));
        mmSaida.Lines.Add(' Valor Informado: ' +
          FloatToStr(Procedimento.ValorInformado));
        mmSaida.Lines.Add(' Quantidade Paga: ' +
          FloatToStr(Procedimento.QuantidadePaga));
        mmSaida.Lines.Add(' Unidade Medida Id: ' +
          Procedimento.UnidadeMedidaId);
        mmSaida.Lines.Add(' Valor Pago Procedimento: ' +
          FloatToStr(Procedimento.ValorPagoProcedimento));
        mmSaida.Lines.Add(' Valor Pago Fornecedor: ' +
          FloatToStr(Procedimento.ValorPagoFornecedor));
        mmSaida.Lines.Add(' Cnpj Fornecedor: ' + Procedimento.CnpjFornecedor);
        mmSaida.Lines.Add(' Valor Coparticipação: ' +
          FloatToStr(Procedimento.ValorCoparticipacao));

        mmSaida.Lines.Add(SLineBreak);

        for var ItemPacote in Procedimento.ItemsPacote do
        begin
          mmSaida.Lines.Add('Item Pacote: ');

          mmSaida.Lines.Add(' Item Pacote Id: ' + ItemPacote.ItemPacoteId);
          mmSaida.Lines.Add(' Guia Procedimento Id: ' +
            ItemPacote.GuiaProcedimentoId);
          mmSaida.Lines.Add(' Código Tabela Pacote Id: ' +
            ItemPacote.CodigoTabelaPacoteId);
          mmSaida.Lines.Add(' Código Procedimento: ' +
            ItemPacote.CodigoProcedimento);
          mmSaida.Lines.Add(' Quantidade: ' +
            FloatToStr(ItemPacote.Quantidade));

          mmSaida.Lines.Add(SLineBreak);
        end;
      end;

    finally
      GuiaMonitoramento.Free;
    end;
  finally
    JSONObj.Free;
  end;
end;

procedure TForm1.btmParseJSONArrayClick(Sender: TObject);
var
  GuiaList: TObjectList<TGuiaMonitoramento>;
  JSONConverter: TJSONDynConverter;
  JSONArray: TJSONArray;
begin
  // Parse da string para um JsonArray
  JSONArray := TJSONObject.ParseJSONValue(mmEntrada2.Text) as TJSONArray;
  try
    GuiaList := TObjectList<TGuiaMonitoramento>.Create;
    try
      JSONConverter := TJSONDynConverter.Create;
      JSONConverter.JSONArrayToObjectList<TGuiaMonitoramento>(JSONArray,
        GuiaList);
      JSONConverter.Free;

      // Limpa a saída antes de colocar novos dados
      mmSaida2.Clear;

      for var GuiaMonitoramento in GuiaList do
      begin
        // Popula o objeto com os dados do json
        mmSaida2.Lines.Add('Guia: ');
        mmSaida2.Lines.Add('GuiaId: ' + GuiaMonitoramento.GuiaId);
        mmSaida2.Lines.Add('RegistroAns: ' + GuiaMonitoramento.RegistroAns);
        mmSaida2.Lines.Add('TipoRegistroId: ' +
          GuiaMonitoramento.TipoRegistroId);
        mmSaida2.Lines.Add('VersaoTissPrestador: ' +
          GuiaMonitoramento.VersaoTissPrestador);
        mmSaida2.Lines.Add('FormaEnvioId: ' + GuiaMonitoramento.FormaEnvioId);
        mmSaida2.Lines.Add('PrestadorCnesId: ' +
          GuiaMonitoramento.PrestadorCnesId);
        mmSaida2.Lines.Add('PrestadorIndicadorIdentificacaoId: ' +
          GuiaMonitoramento.PrestadorIndicadorIdentificacaoId);
        mmSaida2.Lines.Add('PrestadorCnpjCpf: ' +
          GuiaMonitoramento.PrestadorCnpjCpf);
        mmSaida2.Lines.Add('PrestadorMunicipioId: ' +
          GuiaMonitoramento.PrestadorMunicipioId);
        mmSaida2.Lines.Add('BeneficiarioCns: ' +
          GuiaMonitoramento.BeneficiarioCns);
        mmSaida2.Lines.Add('BeneficiarioCpf: ' +
          GuiaMonitoramento.BeneficiarioCpf);
        mmSaida2.Lines.Add('BeneficiarioSexoId: ' +
          GuiaMonitoramento.BeneficiarioSexoId);
        mmSaida2.Lines.Add('BeneficiarioDataNascimento: ' +
          DateToStr(GuiaMonitoramento.BeneficiarioDataNascimento));
        mmSaida2.Lines.Add('BeneficiarioMunicipioId: ' +
          GuiaMonitoramento.BeneficiarioMunicipioId);
        mmSaida2.Lines.Add('BeneficiarioNumeroRegistroPlano: ' +
          GuiaMonitoramento.BeneficiarioNumeroRegistroPlano);
        mmSaida2.Lines.Add('TipoGuiaId: ' + GuiaMonitoramento.TipoGuiaId);
        mmSaida2.Lines.Add('OrigemGuiaId: ' + GuiaMonitoramento.OrigemGuiaId);
        mmSaida2.Lines.Add('Competencia: ' + GuiaMonitoramento.Competencia);
        mmSaida2.Lines.Add('NumeroGuiaPrestador: ' +
          GuiaMonitoramento.NumeroGuiaPrestador);
        mmSaida2.Lines.Add('NumeroGuiaOperadora: ' +
          GuiaMonitoramento.NumeroGuiaOperadora);
        mmSaida2.Lines.Add('IdentificadorReembolso: ' +
          GuiaMonitoramento.IdentificadorReembolso);
        mmSaida2.Lines.Add('IdentificacaoValorPreestabelecido: ' +
          GuiaMonitoramento.IdentificacaoValorPreestabelecido);
        mmSaida2.Lines.Add('GuiaSolicitacaoInternacao: ' +
          GuiaMonitoramento.GuiaSolicitacaoInternacao);
        mmSaida2.Lines.Add('DataSolicitacao: ' +
          DateToStr(GuiaMonitoramento.DataSolicitacao));
        mmSaida2.Lines.Add('NumeroGuiaSpsadtPrincipal: ' +
          GuiaMonitoramento.NumeroGuiaSpsadtPrincipal);
        mmSaida2.Lines.Add('DataAutorizacao: ' +
          DateToStr(GuiaMonitoramento.DataAutorizacao));
        mmSaida2.Lines.Add('DataRealizacao: ' +
          DateToStr(GuiaMonitoramento.DataRealizacao));
        mmSaida2.Lines.Add('DataProtocoloCobranca: ' +
          DateToStr(GuiaMonitoramento.DataProtocoloCobranca));
        mmSaida2.Lines.Add('DataPagamento: ' +
          DateToStr(GuiaMonitoramento.DataPagamento));
        mmSaida2.Lines.Add('DataProcessamentoGuia: ' +
          DateToStr(GuiaMonitoramento.DataProcessamentoGuia));
        mmSaida2.Lines.Add('DataFimPeriodo: ' +
          DateToStr(GuiaMonitoramento.DataFimPeriodo));
        mmSaida2.Lines.Add('DataInicialFaturamento: ' +
          DateToStr(GuiaMonitoramento.DataInicialFaturamento));
        mmSaida2.Lines.Add('ValorTotalInformado: ' +
          FloatToStr(GuiaMonitoramento.ValorTotalInformado));
        mmSaida2.Lines.Add('ValorProcessado: ' +
          FloatToStr(GuiaMonitoramento.ValorProcessado));
        mmSaida2.Lines.Add('ValorTotalPagoProcedimento: ' +
          FloatToStr(GuiaMonitoramento.ValorTotalPagoProcedimento));
        mmSaida2.Lines.Add('ValorTotalDiaria: ' +
          FloatToStr(GuiaMonitoramento.ValorTotalDiaria));
        mmSaida2.Lines.Add('ValorTotalTaxa: ' +
          FloatToStr(GuiaMonitoramento.ValorTotalTaxa));
        mmSaida2.Lines.Add('ValorTotalMaterial: ' +
          FloatToStr(GuiaMonitoramento.ValorTotalMaterial));
        mmSaida2.Lines.Add('ValorTotalOpme: ' +
          FloatToStr(GuiaMonitoramento.ValorTotalOpme));
        mmSaida2.Lines.Add('ValorTotalMedicamento: ' +
          FloatToStr(GuiaMonitoramento.ValorTotalMedicamento));
        mmSaida2.Lines.Add('ValorGlosaGuia: ' +
          FloatToStr(GuiaMonitoramento.ValorGlosaGuia));
        mmSaida2.Lines.Add('ValorPagoGuia: ' +
          FloatToStr(GuiaMonitoramento.ValorPagoGuia));
        mmSaida2.Lines.Add('ValorPagoFornecedor: ' +
          FloatToStr(GuiaMonitoramento.ValorPagoFornecedor));
        mmSaida2.Lines.Add('ValorTotalPagoTabelaPropria: ' +
          FloatToStr(GuiaMonitoramento.ValorTotalPagoTabelaPropria));
        mmSaida2.Lines.Add('ValorTotalPagoCoparticipacao: ' +
          FloatToStr(GuiaMonitoramento.ValorTotalPagoCoparticipacao));
        mmSaida2.Lines.Add('IndicacaoRecemNato: ' +
          GuiaMonitoramento.IndicacaoRecemNato);
        mmSaida2.Lines.Add('IndicacaoAcidente: ' +
          GuiaMonitoramento.IndicacaoAcidente);
        mmSaida2.Lines.Add('CaraterAtendimento: ' +
          GuiaMonitoramento.CaraterAtendimento);
        mmSaida2.Lines.Add('TipoInternacao: ' +
          GuiaMonitoramento.TipoInternacao);
        mmSaida2.Lines.Add('RegimeInternacao: ' +
          GuiaMonitoramento.RegimeInternacao);
        mmSaida2.Lines.Add('SaudeOcupacional: ' +
          GuiaMonitoramento.SaudeOcupacional);
        mmSaida2.Lines.Add('TipoFaturamento: ' +
          GuiaMonitoramento.TipoFaturamento);
        mmSaida2.Lines.Add('DiariaAcompanhante: ' +
          IntToStr(GuiaMonitoramento.DiariaAcompanhante));
        mmSaida2.Lines.Add('DiariaUti: ' +
          IntToStr(GuiaMonitoramento.DiariaUti));
        mmSaida2.Lines.Add('MotivoSaidaId: ' + GuiaMonitoramento.MotivoSaidaId);
        mmSaida2.Lines.Add('CboExecutante: ' + GuiaMonitoramento.CboExecutante);
        mmSaida2.Lines.Add('TipoConsulta: ' + GuiaMonitoramento.TipoConsulta);
        mmSaida2.Lines.Add('RegistroAnsOperadoraIntermediaria: ' +
          GuiaMonitoramento.RegistroAnsOperadoraIntermediaria);
        mmSaida2.Lines.Add('TipoAtendimentoIntermediarioId: ' +
          GuiaMonitoramento.TipoAtendimentoIntermediarioId);
        mmSaida2.Lines.Add('TipoAtendimentoId: ' +
          GuiaMonitoramento.TipoAtendimentoId);
        mmSaida2.Lines.Add('RegimeAtendimentoId: ' +
          GuiaMonitoramento.RegimeAtendimentoId);
        mmSaida2.Lines.Add('FormasRemuneracaoId: ' +
          GuiaMonitoramento.FormasRemuneracaoId);
        mmSaida2.Lines.Add('ValorRemuneracaoId: ' +
          FloatToStr(GuiaMonitoramento.ValorRemuneracaoId));
        mmSaida2.Lines.Add('StatusAnsId: ' + GuiaMonitoramento.StatusAnsId);
        mmSaida2.Lines.Add('GuiaPendente: ' + GuiaMonitoramento.GuiaPendente);
        mmSaida2.Lines.Add('StatusEnviaAns: ' +
          GuiaMonitoramento.StatusEnviaAns);
        mmSaida2.Lines.Add('ChaveAcesso: ' + GuiaMonitoramento.ChaveAcesso);
        mmSaida2.Lines.Add(SLineBreak);

        for var FormaRemuneracao in GuiaMonitoramento.FormasRemuneracao do
        begin
          mmSaida2.Lines.Add('Forma Remuneração: ');

          mmSaida2.Lines.Add(' Guia Forma Remuneração Id: ' +
            FormaRemuneracao.GuiaFormaRemuneracaoId);
          mmSaida2.Lines.Add(' Guia Id: ' + FormaRemuneracao.GuiaId);
          mmSaida2.Lines.Add(' Forma Remuneração Id: ' +
            FormaRemuneracao.FormaRemuneracaoId);
          mmSaida2.Lines.Add(' Valor Remuneração: ' +
            FloatToStr(FormaRemuneracao.ValorRemuneracao));

          mmSaida2.Lines.Add(SLineBreak);
        end;

        for var DiagnosticoCid10 in GuiaMonitoramento.DiagnosticosCid10 do
        begin
          mmSaida2.Lines.Add('Diagnóstico CID-10: ');

          mmSaida2.Lines.Add(' CID-10 Id: ' + DiagnosticoCid10.Cid10Id);
          mmSaida2.Lines.Add(' Sequência: ' +
            IntToStr(DiagnosticoCid10.Sequencia));

          mmSaida2.Lines.Add(SLineBreak);
        end;

        for var DeclaracaoNascido in GuiaMonitoramento.DeclaracoesNascido do
        begin
          mmSaida2.Lines.Add('Declaração Nascido: ');

          mmSaida2.Lines.Add(' Declaração Nascido Id: ' +
            DeclaracaoNascido.DeclaracaoNascidoId);
          mmSaida2.Lines.Add(' Guia Id: ' + DeclaracaoNascido.GuiaId);
          mmSaida2.Lines.Add(' Sequência: ' +
            IntToStr(DeclaracaoNascido.Sequencia));
          mmSaida2.Lines.Add(' Numero Declaração: ' +
            DeclaracaoNascido.NumeroDeclaracao);

          mmSaida2.Lines.Add(SLineBreak);
        end;

        for var DeclaracaoObito in GuiaMonitoramento.DeclaracoesObito do
        begin
          mmSaida2.Lines.Add('Declaração Óbito: ');

          mmSaida2.Lines.Add(' Declaração Óbito Id: ' +
            DeclaracaoObito.DeclaracaoObitoId);
          mmSaida2.Lines.Add(' Guia Id: ' + DeclaracaoObito.GuiaId);
          mmSaida2.Lines.Add(' Sequência: ' +
            IntToStr(DeclaracaoObito.Sequencia));
          mmSaida2.Lines.Add(' Numero Declaração: ' +
            DeclaracaoObito.NumeroDeclaracao);

          mmSaida2.Lines.Add(SLineBreak);
        end;

        for var Procedimento in GuiaMonitoramento.Procedimentos do
        begin
          mmSaida2.Lines.Add('Procedimento: ');

          mmSaida2.Lines.Add(' Procedimento Id: ' +
            Procedimento.ProcedimentoId);
          mmSaida2.Lines.Add(' Guia Id: ' + Procedimento.GuiaId);
          mmSaida2.Lines.Add(' Sequência: ' + IntToStr(Procedimento.Sequencia));
          mmSaida2.Lines.Add(' Código Tabela Monitor Id: ' +
            Procedimento.CodigoTabelaMonitorId);
          mmSaida2.Lines.Add(' Grupo Procedimento Id: ' +
            Procedimento.GrupoProcedimentoId);
          mmSaida2.Lines.Add(' Código Procedimento Id: ' +
            Procedimento.CodigoProcedimentoId);
          mmSaida2.Lines.Add(' Dente Id: ' + Procedimento.DenteId);
          mmSaida2.Lines.Add(' Região Boca Id: ' + Procedimento.RegiaoBocaId);
          mmSaida2.Lines.Add(' Dente Face Id: ' + Procedimento.DenteFaceId);
          mmSaida2.Lines.Add(' Quantidade Informada: ' +
            FloatToStr(Procedimento.QuantidadeInformada));
          mmSaida2.Lines.Add(' Valor Informado: ' +
            FloatToStr(Procedimento.ValorInformado));
          mmSaida2.Lines.Add(' Quantidade Paga: ' +
            FloatToStr(Procedimento.QuantidadePaga));
          mmSaida2.Lines.Add(' Unidade Medida Id: ' +
            Procedimento.UnidadeMedidaId);
          mmSaida2.Lines.Add(' Valor Pago Procedimento: ' +
            FloatToStr(Procedimento.ValorPagoProcedimento));
          mmSaida2.Lines.Add(' Valor Pago Fornecedor: ' +
            FloatToStr(Procedimento.ValorPagoFornecedor));
          mmSaida2.Lines.Add(' Cnpj Fornecedor: ' +
            Procedimento.CnpjFornecedor);
          mmSaida2.Lines.Add(' Valor Coparticipação: ' +
            FloatToStr(Procedimento.ValorCoparticipacao));

          mmSaida2.Lines.Add(SLineBreak);

          for var ItemPacote in Procedimento.ItemsPacote do
          begin
            mmSaida2.Lines.Add('Item Pacote: ');

            mmSaida2.Lines.Add(' Item Pacote Id: ' + ItemPacote.ItemPacoteId);
            mmSaida2.Lines.Add(' Guia Procedimento Id: ' +
              ItemPacote.GuiaProcedimentoId);
            mmSaida2.Lines.Add(' Código Tabela Pacote Id: ' +
              ItemPacote.CodigoTabelaPacoteId);
            mmSaida2.Lines.Add(' Código Procedimento: ' +
              ItemPacote.CodigoProcedimento);
            mmSaida2.Lines.Add(' Quantidade: ' +
              FloatToStr(ItemPacote.Quantidade));

            mmSaida2.Lines.Add(SLineBreak);
          end;
        end;
      end;

    finally
      GuiaList.Free;
    end;
  finally
    JSONArray.Free;
  end;
end;

// OBJETO PARA JSON
procedure TForm1.btObjToJsonClick(Sender: TObject);
var
  // Variaveis para popular objeto com json
  Loja: TLoja;
  JSONEntrada: TJSONObject;

  // Variaveis de conversão de objeto para json
  JSONObject: TJSONObject;
  JSONConverter: TJSONDynConverter;
begin
  JSONEntrada := TJSONObject.ParseJSONValue(mmEntrada3.Text) as TJSONObject;
  try
    Loja := TLoja.Create;
    try
      JSONConverter := TJSONDynConverter.Create;
      JSONConverter.JSONToObject(JSONEntrada, Loja);

      JSONObject := TJSONObject.Create;
      JSONConverter.ObjectToJSONObject(Loja, JSONObject);
      JSONConverter.Free;
      try
        mmSaida3.Clear;
        mmSaida3.Lines.Add(TJSONObject.ParseJSONValue(JSONObject.ToString)
          .Format());
      finally
        JSONObject.Free;
      end;
    finally
      Loja.Free;
    end;
  finally
    JSONEntrada.Free;
  end;
end;

procedure TForm1.btmObjectListToJSONArrayClick(Sender: TObject);
var
  GuiaList: TObjectList<TGuiaMonitoramento>;
  JSONArray: TJSONArray;

  JSONEntrada: TJSONArray;
  JSONConverter: TJSONDynConverter;
begin
  JSONEntrada := TJSONObject.ParseJSONValue(mmEntrada4.Text) as TJSONArray;

  GuiaList := TObjectList<TGuiaMonitoramento>.Create;
  try
    JSONConverter := TJSONDynConverter.Create;
    JSONConverter.JSONArrayToObjectList<TGuiaMonitoramento>(JSONEntrada,
      GuiaList);

    // Preenche a lista GuiaList com objetos do tipo TGuia
    JSONConverter.ObjectListToJSONArray<TGuiaMonitoramento>(GuiaList,
      JSONArray); // Converte a lista de objetos para JSONArray
    JSONConverter.Free;

    mmSaida4.Lines.Add(TJSONObject.ParseJSONValue(JSONArray.ToString)
      .Format());
  finally
    JSONArray.Free; // Libere o JSONArray após o uso
    GuiaList.Free;
    JSONEntrada.Free;
  end;
end;

end.
