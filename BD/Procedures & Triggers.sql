-- Consulta
Select @total_vendas;
Select @Idade;

Select count(*) As total_vendas
From Venda
Where Year(DataVenda) = 2023;

Select *
From Cliente;

Select Format(123.45678,6) as col;

-- Drop dos Procedimentos
Drop Procedure GetAnoVenda;
Drop Procedure GetIdadeCliente;

-- Call dos Procedimentos
Call UpdateProduto(1, 4);
Call UpdateProduto(2, 13);

Call GetAnoVenda(2023, @total_vendas);

Call GetIdadeCliente(256187657, @Idade);

-- "Call" das functions
Select GetIdade(256187657);


-- Procedure para alterar preço de um certo produto
Delimiter $$
Create Procedure UpdateProduto (In CodProduto Int, In NewCusto Int)
Begin
	Update Produto Set Custo = NewCusto Where CodProduto = Codigo;
End $$

-- Stored Procedure para consultar as vendas num determinado ano
Delimiter $$
Create Procedure GetAnoVenda (In ano Int, Out total_vendas Int)
Begin
	Select count(*) Into total_vendas
    From Venda
    Where Year(DataVenda) = ano;
End;
$$

-- Stored Procedure que dado um paciente, indica a idade do mesmo
Delimiter $$
Create Procedure GetIdadeCliente (In IdCliente Int, Out Idade Int)
Begin
	Select timestampdiff(year, DataNascimento, curdate()) Into Idade
	From Cliente
    Where numContribuinte = IdCliente;
End;
$$

-- Function que faz o mesmo que o procedure acima
Delimiter $$
Create Function GetIdade(IdCliente Int)
Returns Int
Begin
	Declare Idade Int;
	Select timestampdiff(year, DataNascimento, curdate()) Into Idade
    From Cliente
    Where numContribuinte = IdCliente;
    Return Idade;
End
$$


-- Trigger que na inserção de um cliente calcula e atualiza a coluna idade
Delimiter $$
Create Trigger SetIdade
Before Insert
On Cliente For Each Row
	set new.Idade = timestampdiff(year, DataNascimento, curdate());
$$

-- Trigger que altera a tabela dos produtos para quando chegar o stock chegar a zero, que poe uma tag a 1 se estiver esgotado
-- Procedimento que atualizar o lucro dos produtos (na tabela produto)

