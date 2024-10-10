object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 388
  ClientWidth = 619
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 619
    Height = 388
    ActivePage = tabParseJSON
    Align = alClient
    TabOrder = 0
    ExplicitWidth = 615
    ExplicitHeight = 387
    object tabParseJSON: TTabSheet
      Caption = 'Parse JSON'
      object pnlParseJSON: TPanel
        Left = 0
        Top = 0
        Width = 611
        Height = 360
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 607
        ExplicitHeight = 359
        object Splitter1: TSplitter
          Left = 305
          Top = 20
          Height = 311
          ExplicitLeft = 345
          ExplicitTop = 17
          ExplicitHeight = 358
        end
        object Splitter2: TSplitter
          Left = 1
          Top = 331
          Width = 609
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          ExplicitLeft = 289
          ExplicitTop = 1
          ExplicitWidth = 358
        end
        object Splitter3: TSplitter
          Left = 1
          Top = 17
          Width = 609
          Height = 3
          Cursor = crVSplit
          Align = alTop
          ExplicitLeft = 305
          ExplicitTop = 14
          ExplicitWidth = 317
        end
        object mmEntrada: TMemo
          Left = 1
          Top = 20
          Width = 304
          Height = 311
          Align = alLeft
          Lines.Strings = (
            '{'
            '  "GuiaId": "12345",'
            '  "RegistroAns": "987654321",'
            '  "TipoRegistroId": "1",'
            '  "VersaoTissPrestador": "3.02.00",'
            '  "FormaEnvioId": "1",'
            '  "PrestadorCnesId": "1234567",'
            '  "PrestadorIndicadorIdentificacaoId": "2",'
            '  "PrestadorCnpjCpf": "12345678901234",'
            '  "PrestadorMunicipioId": "1100023",'
            '  "BeneficiarioCns": "123456789012345",'
            '  "BeneficiarioCpf": "98765432100",'
            '  "BeneficiarioSexoId": "M",'
            '  "BeneficiarioDataNascimento": "1990-05-12",'
            '  "BeneficiarioMunicipioId": "1100031",'
            '  "BeneficiarioNumeroRegistroPlano": "999888777",'
            '  "TipoGuiaId": "2",'
            '  "OrigemGuiaId": "3",'
            '  "Competencia": "2024-09",'
            '  "NumeroGuiaPrestador": "5555",'
            '  "NumeroGuiaOperadora": "4444",'
            '  "IdentificadorReembolso": "N",'
            '  "IdentificacaoValorPreestabelecido": "S",'
            '  "GuiaSolicitacaoInternacao": "999999",'
            '  "DataSolicitacao": "2024-10-01",'
            '  "NumeroGuiaSpsadtPrincipal": "123456",'
            '  "DataAutorizacao": "2024-10-02",'
            '  "DataRealizacao": "2024-10-03",'
            '  "DataProtocoloCobranca": "2024-10-05",'
            '  "DataPagamento": "2024-10-08",'
            '  "DataProcessamentoGuia": "2024-10-09",'
            '  "DataFimPeriodo": "2024-10-06",'
            '  "DataInicialFaturamento": "2024-10-07",'
            '  "ValorTotalInformado": 500.0,'
            '  "ValorProcessado": 480.0,'
            '  "ValorTotalPagoProcedimento": 450.0,'
            '  "ValorTotalDiaria": 100.0,'
            '  "ValorTotalTaxa": 50.0,'
            '  "ValorTotalMaterial": 30.0,'
            '  "ValorTotalOpme": 25.0,'
            '  "ValorTotalMedicamento": 20.0,'
            '  "ValorGlosaGuia": 5.0,'
            '  "ValorPagoGuia": 475.0,'
            '  "ValorPagoFornecedor": 400.0,'
            '  "ValorTotalPagoTabelaPropria": 450.0,'
            '  "ValorTotalPagoCoparticipacao": 50.0,'
            '  "IndicacaoRecemNato": "N",'
            '  "IndicacaoAcidente": "S",'
            '  "CaraterAtendimento": "Eletivo",'
            '  "TipoInternacao": "Cir'#250'rgica",'
            '  "RegimeInternacao": "Parcial",'
            '  "SaudeOcupacional": "N",'
            '  "TipoFaturamento": "Completo",'
            '  "DiariaAcompanhante": 1,'
            '  "DiariaUti": 0,'
            '  "MotivoSaidaId": "Alta",'
            '  "CboExecutante": "225142",'
            '  "TipoConsulta": "Especialidade",'
            '  "RegistroAnsOperadoraIntermediaria": "654321",'
            '  "TipoAtendimentoIntermediarioId": "1",'
            '  "TipoAtendimentoId": "1",'
            '  "RegimeAtendimentoId": "1",'
            '  "FormasRemuneracaoId": "1",'
            '  "ValorRemuneracaoId": 100.0,'
            '  "StatusAnsId": "Pendente",'
            '  "GuiaPendente": "S",'
            '  "StatusEnviaAns": "Aguardando",'
            '  "ChaveAcesso": "abcdef123456",'
            '  "FormasRemuneracao": ['
            '    {'
            '      "GuiaFormaRemuneracaoId": "1",'
            '      "GuiaId": "12345",'
            '      "FormaRemuneracaoId": "2",'
            '      "ValorRemuneracao": 100.0'
            '    }'
            '  ],'
            '  "DiagnosticosCid10": ['
            '    {'
            '      "Cid10Id": "A00",'
            '      "Sequencia": 1'
            '    }'
            '  ],'
            '  "DeclaracoesNascido": ['
            '    {'
            '      "DeclaracaoNascidoId": "1",'
            '      "GuiaId": "12345",'
            '      "Sequencia": 1,'
            '      "NumeroDeclaracao": "123456789"'
            '    }'
            '  ],'
            '  "DeclaracoesObito": ['
            '    {'
            '      "DeclaracaoObitoId": "1",'
            '      "GuiaId": "12345",'
            '      "Sequencia": 1,'
            '      "NumeroDeclaracao": "987654321"'
            '    }'
            '  ],'
            '  "Procedimentos": ['
            '    {'
            '      "ProcedimentoId": "1",'
            '      "GuiaId": "12345",'
            '      "Sequencia": 1,'
            '      "GrupoProcedimentoId": "10",'
            '      "CodigoProcedimentoId": "20",'
            '      "CodigoTabelaMonitorId": "30",'
            '      "DenteId": "D18",'
            '      "RegiaoBocaId": "RB1",'
            '      "DenteFaceId": "DF2",'
            '      "QuantidadeInformada": 1.0,'
            '      "ValorInformado": 500.0,'
            '      "QuantidadePaga": 1.0,'
            '      "UnidadeMedidaId": "UN",'
            '      "ValorPagoProcedimento": 450.0,'
            '      "ValorPagoFornecedor": 400.0,'
            '      "CnpjFornecedor": "12345678901234",'
            '      "ValorCoparticipacao": 50.0,'
            '      "ItemsPacote": ['
            '        {'
            '          "ItemPacoteId": "1",'
            '          "GuiaProcedimentoId": "1",'
            '          "CodigoTabelaPacoteId": "40",'
            '          "CodigoProcedimento": "50",'
            '          "Quantidade": 1.0'
            '        }'
            '      ]'
            '    }'
            '  ]'
            '}')
          ScrollBars = ssVertical
          TabOrder = 0
          ExplicitHeight = 310
        end
        object mmSaida: TMemo
          Left = 308
          Top = 20
          Width = 302
          Height = 311
          Align = alClient
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 1
          ExplicitWidth = 298
          ExplicitHeight = 310
        end
        object btmParse: TButton
          Left = 1
          Top = 334
          Width = 609
          Height = 25
          Align = alBottom
          Caption = 'Parse JSON'
          TabOrder = 2
          OnClick = btmParseClick
          ExplicitTop = 333
          ExplicitWidth = 605
        end
        object pnlLabelParse: TPanel
          Left = 1
          Top = 1
          Width = 609
          Height = 16
          Align = alTop
          TabOrder = 3
          ExplicitWidth = 605
          object lblEntrada: TLabel
            Left = 121
            Top = 0
            Width = 40
            Height = 13
            AutoSize = False
            Caption = 'Entrada'
          end
          object lblSaida: TLabel
            Left = 447
            Top = 0
            Width = 26
            Height = 13
            Alignment = taRightJustify
            Caption = 'Sa'#237'da'
          end
        end
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Object to JSON'
      ImageIndex = 1
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 611
        Height = 360
        Align = alClient
        Caption = 'Panel1'
        TabOrder = 0
        object Splitter4: TSplitter
          Left = 305
          Top = 20
          Height = 311
          ExplicitLeft = 345
          ExplicitTop = 17
          ExplicitHeight = 358
        end
        object Splitter5: TSplitter
          Left = 1
          Top = 331
          Width = 609
          Height = 3
          Cursor = crVSplit
          Align = alBottom
          Beveled = True
          Visible = False
          ExplicitLeft = 289
          ExplicitTop = 1
          ExplicitWidth = 358
        end
        object Splitter6: TSplitter
          Left = 1
          Top = 17
          Width = 609
          Height = 3
          Cursor = crVSplit
          Align = alTop
          Beveled = True
          Visible = False
          ExplicitLeft = 305
          ExplicitTop = 14
          ExplicitWidth = 317
        end
        object mmEntrada2: TMemo
          Left = 1
          Top = 20
          Width = 304
          Height = 311
          Align = alLeft
          Lines.Strings = (
            '{'
            '  "Nome":"Loja A",'
            '  "Produtos":'
            '  ['
            '    {'
            '      "Nome":"Produto 1",'
            '      "Preco":10.1'
            '    },'
            '    {'
            '      "Nome":"Produto 2",'
            '      "Preco":20.2'
            '    },'
            '    {'
            '      "Nome":"Produto 3",'
            '      "Preco":30.3'
            '    },'
            '    {'
            '      "Nome":"Produto 4",'
            '      "Preco":40.4'
            '    },'
            '    {'
            '      "Nome":"Produto 5",'
            '      "Preco":50.5'
            '    },'
            '    {'
            '      "Nome":"Produto 6",'
            '      "Preco":60.6'
            '    },'
            '    {'
            '      "Nome":"Produto 7",'
            '      "Preco":70.7'
            '    },'
            '    {'
            '      "Nome":"Produto 8",'
            '      "Preco":80.8'
            '    },'
            '    {'
            '      "Nome":"Produto 9",'
            '      "Preco":90.9'
            '    },'
            '    {'
            '      "Nome":"Produto 10",'
            '      "Preco":100.10'
            '    },'
            '    {'
            '      "Nome":"Produto 11",'
            '      "Preco":110.11'
            '    },'
            '    {'
            '      "Nome":"Produto 12",'
            '      "Preco":120.12'
            '    },'
            '    {'
            '      "Nome":"Produto 13",'
            '      "Preco":130.13'
            '    },'
            '    {'
            '      "Nome":"Produto 14",'
            '      "Preco":140.14'
            '    },'
            '    {'
            '      "Nome":"Produto 15",'
            '      "Preco":150.15'
            '    },'
            '    {'
            '      "Nome":"Produto 16",'
            '      "Preco":160.16'
            '    },'
            '    {'
            '      "Nome":"Produto 17",'
            '      "Preco":170.17'
            '    },'
            '    {'
            '      "Nome":"Produto 18",'
            '      "Preco":180.18'
            '    },'
            '    {'
            '      "Nome":"Produto 19",'
            '      "Preco":190.19'
            '    },'
            '    {'
            '      "Nome":"Produto 20",'
            '      "Preco":200.20'
            '    }'
            '  ]'
            '}')
          ScrollBars = ssVertical
          TabOrder = 0
        end
        object mmSaida2: TMemo
          Left = 308
          Top = 20
          Width = 302
          Height = 311
          Align = alClient
          ReadOnly = True
          ScrollBars = ssVertical
          TabOrder = 1
        end
        object btmConvert: TButton
          Left = 1
          Top = 334
          Width = 609
          Height = 25
          Align = alBottom
          Caption = 'Object to JSON'
          TabOrder = 2
          OnClick = btObjToJsonClick
        end
        object pnlLabelObj: TPanel
          Left = 1
          Top = 1
          Width = 609
          Height = 16
          Align = alTop
          TabOrder = 3
          object Label1: TLabel
            Left = 121
            Top = 0
            Width = 40
            Height = 13
            AutoSize = False
            Caption = 'Entrada'
          end
          object Label2: TLabel
            Left = 447
            Top = 0
            Width = 26
            Height = 13
            Alignment = taRightJustify
            Caption = 'Sa'#237'da'
          end
        end
      end
    end
  end
end
