# 📊 Análise de Óbitos por Causas Externas no Brasil (2010–2020)

Este projeto tem como objetivo analisar os **óbitos por causas externas no Brasil**, no período de **2010 a 2020**, utilizando dados públicos do **DATASUS**. O foco principal está na investigação dos padrões de mortalidade associados à **ocupação de empregados(as) domésticos(as)**, considerando recortes por **gênero, idade, raça, ocupação e causa do óbito**, além da análise temporal dos dados.

---

## 🗂️ Base de Dados

- **Fonte:** DATASUS – Óbitos por Causas Externas (DOEXT)
- **Período:** 2010 a 2020
- **Base consolidada:** 1.656.042 registros
- **Número de variáveis:** 97

### Variáveis principais
- Idade  
- Sexo  
- Raça/Cor  
- Ocupação  
- Causa básica do óbito (CID-10)  
- Acidente de trabalho  
- Data do óbito  

---

## 🔎 Metodologia

1. Consolidação das bases anuais DOEXT em um único dataset.
2. Filtragem dos registros de empregados(as) domésticos(as):
   - Arrumador
   - Diarista
   - Faxineiro
   - Serviços Gerais
3. Aplicação de filtros adicionais:
   - Faixa etária entre **14 e 75 anos**
   - Análise por gênero
4. Análises realizadas:
   - Identificação das **principais causas de óbitos por causas externas**
   - Comparação entre gêneros
   - Distribuição por raça, idade e ocupação
   - Análise temporal para verificação de tendências e sazonalidade

---

## 📈 Principais Resultados

- As causas de óbito mais frequentes estão associadas principalmente a:
  - **Acidentes de trânsito** (pedestres, motociclistas e ocupantes de veículos)
  - **Quedas**, **choques elétricos** e outros acidentes não especificados
  - **Agressões** e **lesões autoprovocadas**, especialmente no recorte feminino
- As ocupações com maior número de óbitos foram **Serviços Gerais** e **Diarista**.
- Observou-se predominância de óbitos entre pessoas **brancas e pardas**.
- A análise por gênero sugere que homens apresentam maior número de óbitos em acidentes de trabalho, possivelmente devido à exposição a atividades mais perigosas.
- Na análise temporal, foram identificadas variações ao longo dos anos, porém **sem evidência clara de sazonalidade** para a maioria das causas.

---

## 🧠 Conclusões

Os resultados evidenciam a **precarização e a falta de segurança no trabalho doméstico no Brasil**, refletidas no elevado número de óbitos por causas externas. Também foi observada possível **subnotificação ocupacional**, além de desigualdades de gênero na exposição aos riscos de trabalho.

Como trabalhos futuros, recomenda-se:
- Desenvolvimento de modelos explicativos ou preditivos
- Análises regionais
- Integração com dados socioeconômicos e políticas públicas

---

## 📚 Fonte dos Dados

- **DATASUS – Ministério da Saúde**  
  https://datasus.saude.gov.br/
