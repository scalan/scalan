package scalan.meta

import scalan.BaseNestedTests

class SqlParserTests extends BaseNestedTests {
  describe("can parse select") {
    val schema =
      """
        |create table lineitem(
        |   l_orderkey integer,
        |   l_partkey integer,
        |   l_suppkey integer,
        |   l_linenumber integer,
        |   l_quantity real,
        |   l_extendedprice real,
        |   l_discount real,
        |   l_tax real,
        |   l_returnflag char,
        |   l_linestatus char,
        |   l_shipdate date,
        |   l_commitdate date,
        |   l_receiptdate date,
        |   l_shipinstruct varchar,
        |   l_shipmode varchar,
        |   l_comment varchar);
        |
        |
        |create table orders(
        |    o_orderkey integer,
        |    o_custkey integer,
        |    o_orderstatus char,
        |    o_totalprice real,
        |    o_orderdate date,
        |    o_orderpriority varchar,
        |    o_clerk varchar,
        |    o_shippriority integer,
        |    o_comment varchar);
        |
        |create table customer(
        |    c_custkey integer,
        |    c_name varchar,
        |    c_address varchar,
        |    c_nationkey integer,
        |    c_phone varchar,
        |    c_acctbal real,
        |    c_mktsegment varchar,
        |    c_comment varchar);
        |
        |
        |create table supplier(
        |    s_suppkey integer,
        |    s_name varchar,
        |    s_address varchar,
        |    s_nationkey integer,
        |    s_phone varchar,
        |    s_acctbal real,
        |    s_comment varchar);
        |
        |create table partsupp(
        |    ps_partkey integer,
        |    ps_suppkey integer,
        |    ps_availqty integer,
        |    ps_supplycost real,
        |    ps_comment varchar);
        |
        |
        |create table region(
        |    r_regionkey integer,
        |    r_name varchar,
        |    r_comment varchar);
        |
        |create table nation(
        |    n_nationkey integer,
        |    n_name varchar,
        |    n_regionkey integer,
        |    n_comment varchar);
        |
        |create table part(
        |    p_partkey integer,
        |    p_name varchar,
        |    p_mfgr varchar,
        |    p_brand varchar,
        |    p_type varchar,
        |    p_size integer,
        |    p_container varchar,
        |    p_retailprice real,
        |    p_comment varchar);
        |
        |create index lineitem_pk on lineitem(l_orderkey, l_linenumber);
        |create index lineitem_order_fk on lineitem(l_orderkey);
        |create index lineitem_supp_fk on lineitem(l_suppkey);
        |create index lineitem_part_fk on lineitem(l_partkey);
        |create index lineitem_ps_fk on lineitem(l_partkey, l_suppkey);
        |create index part_pk on part(p_partkey);
        |create index supplier_pk on supplier(s_suppkey);
        |create index partsupp_pk on partsupp(ps_partkey, ps_suppkey);
        |create index partsupp_supp_fk on partsupp(ps_suppkey);
        |create index partsupp_part_fk on partsupp(ps_partkey);
        |create index customer_pk on customer(c_custkey);
        |create index customer_nation_fk on customer(c_nationkey);
        |create index orders_pk on orders(o_orderkey);
        |create index orders_cust_fk on orders(o_custkey);
        |create index nation_pk on nation(n_nationkey);
        |create index nation_region_fk on nation(n_regionkey);
        |create index region_pk on region(r_regionkey);
      """.stripMargin

    val queries =
      """
        |-- Q1
        |select
        |    l_returnflag,
        |    l_linestatus,
        |    sum(l_quantity) as sum_qty,
        |    sum(l_extendedprice) as sum_base_price,
        |    sum(l_extendedprice*(1-l_discount)) as sum_disc_price,
        |    sum(l_extendedprice*(1-l_discount)*(1+l_tax)) as sum_charge,
        |    avg(l_quantity) as avg_qty,
        |    avg(l_extendedprice) as avg_price,
        |    avg(l_discount) as avg_disc,
        |    count(*) as count_order
        |from
        |    lineitem
        |where
        |    l_shipdate <= 19981201
        |group by
        |    l_returnflag,
        |    l_linestatus
        |order by
        |    l_returnflag,
        |    l_linestatus;
        |
        |-- Q2:
        |select
        |    s_acctbal,
        |    s_name,
        |    n_name,
        |    p_partkey,
        |    p_mfgr,
        |    s_address,
        |    s_phone,
        |    s_comment
        |from
        |    part join partsupp on p_partkey = ps_partkey
        |    join supplier on ps_suppkey = s_suppkey
        |    join nation on s_nationkey = n_nationkey
        |    join region on n_regionkey = r_regionkey
        |where
        |    p_size = 43
        |    and p_type like '%TIN'
        |    and r_name = 'AFRICA'
        |    and ps_supplycost = (
        |        select
        |            min(ps_supplycost)
        |        from
        |            partsupp join supplier on ps_partkey = s_suppkey
        |            join nation on s_nationkey = n_nationkey
        |            join region on n_regionkey = r_regionkey
        |        where
        |            p_partkey = ps_partkey
        |            and r_name = 'AFRICA')
        |order by
        |    s_acctbal,
        |    n_name,
        |    s_name,
        |    p_partkey;
        |
        |-- Q3
        |select
        |    l_orderkey,
        |    sum(l_extendedprice*(1-l_discount)) as revenue,
        |    o_orderdate,
        |    o_shippriority
        |from
        |    customer join orders on c_custkey = o_custkey
        |    join lineitem on l_orderkey = o_orderkey
        |where
        |    c_mktsegment = 'HOUSEHOLD'
        |    and o_orderdate < 19950304
        |    and l_shipdate > 19950304
        |group by
        |    l_orderkey,
        |    o_orderdate,
        |    o_shippriority
        |order by
        |    revenue desc,
        |    o_orderdate;
        |
        |-- Q4
        |select
        |    o_orderpriority,
        |    count(*) as order_count
        |from
        |    orders join lineitem on l_orderkey = o_orderkey
        |where
        |    o_orderdate >= 19930801
        |    and o_orderdate < 19931101
        |    and l_commitdate < l_receiptdate
        |group by
        |    o_orderpriority
        |order by
        |    o_orderpriority;
        |
        |
        |-- Q5
        |select
        |    n_name,
        |    sum(l_extendedprice * (1-l_discount)) as revenue
        |from
        |    customer join orders on c_custkey = o_custkey
        |    join lineitem on l_orderkey = o_orderkey
        |    join supplier on l_suppkey = s_suppkey
        |    join nation on c_nationkey = n_nationkey
        |    join region on n_regionkey = r_regionkey
        |where
        |    c_nationkey = s_nationkey
        |    and r_name = 'ASIA'
        |    and o_orderdate >= 19960101
        |    and o_orderdate < 19970101
        |group by
        |    n_name
        |order by
        |    revenue desc;
        |
        |
        |-- Q6
        |select
        |    sum(l_extendedprice*l_discount) as revenue
        |from
        |    lineitem
        |where
        |    l_shipdate between 19960101 and 19970101
        |    and l_discount between 0.08 and 0.1
        |    and l_quantity < 24;
        |
        |
        |-- Q7
        |select
        |    supp_nation,
        |    cust_nation,
        |    l_year, sum(volume) as revenue
        |from (
        |    select
        |        n1.n_name as supp_nation,
        |        n2.n_name as cust_nation,
        |        l_shipdate/10000 as l_year,
        |        l_extendedprice * (1-l_discount) as volume
        |    from
        |        supplier join lineitem on s_suppkey = l_suppkey
        |        join orders on o_orderkey = l_orderkey
        |        join customer on c_custkey = o_custkey
        |        join nation as n1 on s_nationkey = n1.n_nationkey
        |        join nation as n2 on c_nationkey = n2.n_nationkey
        |    where
        |        ((n1.n_name = 'UNITED STATES' and n2.n_name = 'INDONESIA')
        |         or (n1.n_name = 'INDONESIA' and n2.n_name = 'UNITED STATES'))
        |         and l_shipdate between 19950101 and 19961231) as shipping
        |group by
        |    supp_nation,
        |    cust_nation,
        |    l_year
        |order by
        |    supp_nation,
        |    cust_nation,
        |    l_year;
        |
        |-- Q8
        |select
        |    o_year,
        |    nation_volume / total_volume as mkt_share
        |from
        |    (select
        |        o_year,
        |        sum(case
        |            when nation = 'INDONESIA'
        |            then volume
        |            else 0 end) as nation_volume,
        |        sum(volume) as total_volume
        |    from (
        |        select
        |            o_orderdate/10000 as o_year,
        |            l_extendedprice * (1-l_discount) as volume,
        |            n2.n_name as nation
        |        from
        |            part join lineitem on p_partkey = l_partkey
        |            join supplier on s_suppkey = l_suppkey
        |            join orders on l_orderkey = o_orderkey
        |            join customer on o_custkey = c_custkey
        |            join nation n1 on c_nationkey = n1.n_nationkey
        |            join nation n2 on s_nationkey = n2.n_nationkey
        |            join region on n1.n_regionkey = r_regionkey
        |        where
        |            r_name = 'ASIA'
        |            and o_orderdate between 19950101 and 19961231
        |            and p_type = 'MEDIUM ANODIZED NICKEL'
        |        ) as all_nations
        |    group by
        |        o_year) as mkt
        |order by
        |    o_year;
        |
        |-- Q9
        |select
        |    nation,
        |    o_year,
        |    sum(amount) as sum_profit
        |from (
        |    select
        |        n_name as nation,
        |        o_orderdate/10000 as o_year,
        |        l_extendedprice*(1-l_discount)-ps_supplycost * l_quantity as amount
        |    from
        |        lineitem join supplier on s_suppkey = l_suppkey
        |        join part on p_partkey = l_partkey
        |        join partsupp on ps_partkey = l_partkey and ps_suppkey = l_suppkey
        |        join orders on o_orderkey = l_orderkey
        |        join nation on s_nationkey = n_nationkey
        |    where
        |        p_name like '%ghost%'
        |    ) as profit
        |group by
        |    nation,
        |    o_year
        |order by
        |    nation,
        |    o_year desc;
        |
        |-- Q10
        |select
        |    c_custkey,
        |    c_name,
        |    sum(l_extendedprice * (1 - l_discount)) as revenue,
        |    c_acctbal,
        |    n_name,
        |    c_address,
        |    c_phone,
        |    c_comment
        |from
        |    orders join customer on c_custkey = o_custkey
        |    join lineitem on l_orderkey = o_orderkey
        |    join nation on c_nationkey = n_nationkey
        |where
        |    o_orderdate >= 19941101 and o_orderdate < 19950201
        |    and l_returnflag = 'R'
        |group by
        |    c_custkey,
        |    c_name,
        |    c_acctbal,
        |    c_phone,
        |    n_name,
        |    c_address,
        |    c_comment
        |order by
        |    revenue desc;
        |
        |-- Q11
        |select
        |    ps_partkey,
        |    sum(ps_supplycost * ps_availqty) as value
        |from
        |    partsupp join supplier on ps_suppkey = s_suppkey
        |    join nation on s_nationkey = n_nationkey
        |where
        |    n_name = 'UNITED KINGDOM'
        |group by
        |    ps_partkey
        |having
        |    value > 0.0001 * (
        |        select
        |            sum(ps_supplycost * ps_availqty)
        |        from
        |            partsupp join supplier on ps_suppkey = s_suppkey
        |            join nation on s_nationkey = n_nationkey
        |        where
        |            n_name = 'UNITED KINGDOM')
        |order by
        |    value desc;
        |
        |-- Q12
        |select
        |    l_shipmode,
        |    sum(case
        |        when o_orderpriority ='1-URGENT'
        |        or o_orderpriority ='2-HIGH'
        |        then 1
        |        else 0
        |        end) as high_line_count,
        |    sum(case
        |        when o_orderpriority <> '1-URGENT'
        |        and o_orderpriority <> '2-HIGH'
        |        then 1
        |        else 0
        |        end) as low_line_count
        |from
        |    orders join lineitem on o_orderkey = l_orderkey
        |where
        |    l_shipmode in ('MAIL', 'SHIP')
        |    and l_commitdate < l_receiptdate
        |    and l_shipdate < l_commitdate
        |    and l_receiptdate >= 19940101
        |    and l_receiptdate < 19950101
        |group by
        |    l_shipmode
        |order by
        |    l_shipmode;
        |
        |
        |-- Q13 (left outer join is not supported)
        |select
        |    c_count,
        |    count(*) as custdist
        |from (
        |    select
        |        c_custkey,
        |        count(o_orderkey) as c_count
        |    from
        |        customer left outer join orders on c_custkey = o_custkey
        |    where
        |        o_comment not like '%unusual%packages%'
        |    group by
        |        c_custkey
        |    ) as c_orders
        |group by
        |    c_count
        |order by
        |    custdist desc,
        |    c_count desc;
        |
        |
        |-- Q14
        |select
        |    100.00 * sum(case
        |              when p_type like 'PROMO%'
        |              then l_extendedprice*(1-l_discount)
        |              else 0 end)
        |        / sum(l_extendedprice * (1 - l_discount)) as promo_revenue
        |    from
        |        lineitem join part on l_partkey = p_partkey
        |    where
        |        l_shipdate >= 19940301
        |        and l_shipdate < 19940401;
        |
        |-- Q15: views are not supported
        |select
        |    s_suppkey,
        |    s_name,
        |    s_address,
        |    s_phone,
        |    total_revenue
        |from
        |    supplier join (
        |    select
        |        l_suppkey as supplier_no,
        |        sum(l_extendedprice * (1 - l_discount)) as total_revenue
        |    from
        |        lineitem
        |    where
        |        l_shipdate >= 19930901
        |        and l_shipdate < 19931201
        |    group by
        |        l_suppkey) as revenue on s_suppkey = supplier_no
        |where
        |    total_revenue = (
        |        select
        |            max(total_revenue)
        |        from
        |            (select
        |                l_suppkey as supplier_no,
        |                sum(l_extendedprice * (1 - l_discount)) as total_revenue
        |            from
        |                lineitem
        |            where
        |                l_shipdate >= 19930901
        |                and l_shipdate < 19931201
        |            group by
        |                l_suppkey) as revenew)
        |order by
        |    s_suppkey;
        |
        |
        |-- Q16: count distinct is not supported,
        |select
        |    p_brand,
        |    p_type,
        |    p_size,
        |    count(distinct ps_suppkey) as supplier_cnt
        |from
        |    partsupp join part on p_partkey = ps_partkey
        |where
        |    p_brand <> 'Brand#21'
        |    and p_type not like 'PROMO PLATED%'
        |    and p_size in (23, 3, 33, 29, 40, 27, 22, 4)
        |    and ps_suppkey not in (
        |        select
        |            s_suppkey
        |        from
        |            supplier
        |        where
        |            s_comment like '%Customer%Complaints%')
        |group by
        |    p_brand,
        |    p_type,
        |    p_size
        |order by
        |    supplier_cnt desc,
        |    p_brand,
        |    p_type,
        |    p_size;
        |
        |-- Q17
        |select
        |    sum(l_extendedprice) / 7.0 as avg_yearly
        |from
        |    lineitem join part on p_partkey = l_partkey
        |where
        |    p_brand = 'Brand#15'
        |    and p_container = 'MED BAG'
        |    and l_quantity < 0.2 * (
        |        select
        |            avg(l_quantity)
        |        from
        |            lineitem
        |        where
        |            l_partkey = p_partkey);
        |
        |-- Q18
        |select
        |    c_name,
        |    c_custkey,
        |    o_orderkey,
        |    o_orderdate,
        |    o_totalprice,
        |    sum(l_quantity)
        |from
        |    customer join orders on  c_custkey = o_custkey
        |    join lineitem on o_orderkey = l_orderkey
        |where
        |    o_orderkey in (
        |        select
        |            l_orderkey
        |        from
        |            (select
        |                l_orderkey,
        |                sum(l_quantity) as sum_quantity
        |            from
        |                lineitem
        |            group by
        |                l_orderkey
        |            having
        |                sum_quantity > 300) as ord)
        |group by
        |    c_name,
        |    c_custkey,
        |    o_orderkey,
        |    o_orderdate,
        |    o_totalprice
        |order by
        |    o_totalprice desc,
        |    o_orderdate;
        |
        |
        |-- Q19
        |select
        |    sum(l_extendedprice * (1 - l_discount)) as revenue
        |from
        |    lineitem join part on p_partkey = l_partkey
        |where
        |    (p_brand = 'Brand#31'
        |    and p_container in ( 'SM CASE', 'SM BOX', 'SM PACK', 'SM PKG')
        |    and l_quantity >= 26 and l_quantity <= 36
        |    and p_size between 1 and 5
        |    and l_shipmode in ('AIR', 'AIR REG')
        |    and l_shipinstruct = 'DELIVER IN PERSON')
        |or
        |    (p_brand = 'Brand#43'
        |    and p_container in ('MED BAG', 'MED BOX', 'MED PKG', 'MED PACK')
        |    and l_quantity >= 15 and l_quantity <= 25
        |    and p_size between 1 and 10
        |    and l_shipmode in ('AIR', 'AIR REG')
        |    and l_shipinstruct = 'DELIVER IN PERSON')
        |or
        |    (p_brand = 'Brand#43'
        |    and p_container in ( 'LG CASE', 'LG BOX', 'LG PACK', 'LG PKG')
        |    and l_quantity >= 4 and l_quantity <= 14
        |    and p_size between 1 and 15
        |    and l_shipmode in ('AIR', 'AIR REG')
        |    and l_shipinstruct = 'DELIVER IN PERSON');
        |
        |-- Q20
        |select
        |    s_name,
        |    s_address
        |from
        |    supplier join nation on s_nationkey = n_nationkey
        |where
        |    s_suppkey in (
        |        select
        |            ps_suppkey
        |        from
        |            partsupp
        |        where
        |            ps_partkey in (
        |                select
        |                    p_partkey
        |                from
        |                    part
        |                where
        |                    p_name like 'azure%')
        |            and ps_availqty > 0.5 * (
        |                select
        |                    sum(l_quantity)
        |                from
        |                    lineitem
        |                where
        |                    l_partkey = ps_partkey
        |                    and l_suppkey = s_suppkey
        |                    and l_shipdate >= 19960101
        |                    and l_shipdate < 19970101))
        |    and n_name = 'JORDAN'
        |order by
        |    s_name;
        |
        |-- Q21:
        |select
        |    s_name,
        |    count(*) as numwait
        |from
        |    supplier join lineitem l1 on s_suppkey = l1.l_suppkey
        |    join orders on o_orderkey = l1.l_orderkey
        |    join nation on s_nationkey = n_nationkey
        |where
        |    o_orderstatus = 'F'
        |    and l1.l_receiptdate > l1.l_commitdate
        |    and exists (
        |        select *
        |        from
        |            lineitem l2
        |        where
        |            l2.l_orderkey = l1.l_orderkey
        |            and l2.l_suppkey <> l1.l_suppkey)
        |    and not exists (
        |        select *
        |        from
        |            lineitem l3
        |        where
        |            l3.l_orderkey = l1.l_orderkey
        |            and l3.l_suppkey <> l1.l_suppkey
        |            and l3.l_receiptdate > l3.l_commitdate)
        |    and n_name = 'MOROCCO'
        |group by
        |    s_name
        |order by
        |    numwait desc,
        |    s_name;
        |
        |-- Q22
        |select
        |    cntrycode,
        |    count(*) as numcust,
        |    sum(c_acctbal) as totacctbal
        |from (
        |    select
        |        substring(c_phone, 1, 2) as cntrycode,
        |        c_acctbal
        |    from
        |        customer
        |    where
        |        substring(c_phone, 1, 2) in ('[I1]','[I2]','[I3]','[I4]','[I5]','[I6]','[I7]')
        |        and c_acctbal > (
        |            select
        |                avg(c_acctbal)
        |            from
        |                customer
        |            where
        |                c_acctbal > 0.00
        |                and substring (c_phone, 1, 2) in ('[I1]','[I2]','[I3]','[I4]','[I5]','[I6]','[I7]'))
        |        and not exists (
        |            select *
        |            from
        |                orders
        |            where
        |                o_custkey = c_custkey)) as custsale
        |group by
        |    cntrycode
        |order by
        |    cntrycode;
      """.stripMargin.split(";").map(_.trim).filter(_.nonEmpty)

    val parser = new SqlParser {}
    parser.parseDDL(schema)

    val ignoredQueries = Set.empty[Int]

    // just verifies that queries can be parsed
    queries.zipWithIndex.foreach { case (sql, i) =>
      it(s"Q${i + 1}") {
        if (ignoredQueries.contains(i + 1)) {
          pendingUntilFixed {
            val parsed = parser.parseSelect(sql)
            println(parsed)
          }
        }
        val parsed = parser.parseSelect(sql)
        println(parsed)
      }
    }
  }
}
