# Anotacoes video

## Arima: 

- Medias moveis: filtro.
- (p, d, q): parametros.
- Previsao

- p, d, q
   - p: ordem (numero de defasagens) do modelo auto regressivo
   - d: grau de diferenciacao (numero de vezes em que os dados tiveram valores passados subtraidos). Serve para remover a sazonalidade.
   - q: ordem do modelo de media movel. Relacionado aos residuos.

- Modelo:
   - Olhar o passado (sinal autoregressivo)
   - Ajuste nos erros (residuos)

   - ARIMA(1,0,0) -> AR(1) -> autoregressivo
   - ARIMA(0, 1, 0) -> I(1) -> integracao
   - ARIMA(0,0,1) -> MA(1) -> lag

## Sarima:

- Sazonalidade.
- (P, D, Q)
