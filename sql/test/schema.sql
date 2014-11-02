--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: fridge; Type: TABLE; Schema: public; Owner: antoine; Tablespace: 
--

CREATE TABLE fridge (
    id integer NOT NULL,
    user_id integer NOT NULL,
    product_id integer NOT NULL
);


ALTER TABLE public.fridge OWNER TO antoine;

--
-- Name: fridge_id_seq; Type: SEQUENCE; Schema: public; Owner: antoine
--

CREATE SEQUENCE fridge_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.fridge_id_seq OWNER TO antoine;

--
-- Name: fridge_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antoine
--

ALTER SEQUENCE fridge_id_seq OWNED BY fridge.id;


--
-- Name: fridge_product_id_seq; Type: SEQUENCE; Schema: public; Owner: antoine
--

CREATE SEQUENCE fridge_product_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.fridge_product_id_seq OWNER TO antoine;

--
-- Name: fridge_product_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antoine
--

ALTER SEQUENCE fridge_product_id_seq OWNED BY fridge.product_id;


--
-- Name: fridge_user_id_seq; Type: SEQUENCE; Schema: public; Owner: antoine
--

CREATE SEQUENCE fridge_user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.fridge_user_id_seq OWNER TO antoine;

--
-- Name: fridge_user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antoine
--

ALTER SEQUENCE fridge_user_id_seq OWNED BY fridge.user_id;


--
-- Name: package; Type: TABLE; Schema: public; Owner: antoine; Tablespace: 
--

CREATE TABLE package (
    id integer NOT NULL
);


ALTER TABLE public.package OWNER TO antoine;

--
-- Name: package_id_seq; Type: SEQUENCE; Schema: public; Owner: antoine
--

CREATE SEQUENCE package_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.package_id_seq OWNER TO antoine;

--
-- Name: package_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antoine
--

ALTER SEQUENCE package_id_seq OWNED BY package.id;


--
-- Name: product; Type: TABLE; Schema: public; Owner: antoine; Tablespace: 
--

CREATE TABLE product (
    id integer NOT NULL,
    rotten timestamp with time zone NOT NULL,
    total_quantity real NOT NULL,
    quantity_unit character(5) NOT NULL,
    package integer NOT NULL,
    product_category integer NOT NULL
);


ALTER TABLE public.product OWNER TO antoine;

--
-- Name: product_category; Type: TABLE; Schema: public; Owner: antoine; Tablespace: 
--

CREATE TABLE product_category (
    id integer NOT NULL,
    category character varying(256) NOT NULL
);


ALTER TABLE public.product_category OWNER TO antoine;

--
-- Name: product_category_id_seq; Type: SEQUENCE; Schema: public; Owner: antoine
--

CREATE SEQUENCE product_category_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.product_category_id_seq OWNER TO antoine;

--
-- Name: product_category_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antoine
--

ALTER SEQUENCE product_category_id_seq OWNED BY product_category.id;


--
-- Name: product_id_seq; Type: SEQUENCE; Schema: public; Owner: antoine
--

CREATE SEQUENCE product_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.product_id_seq OWNER TO antoine;

--
-- Name: product_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antoine
--

ALTER SEQUENCE product_id_seq OWNED BY product.id;


--
-- Name: product_package_seq; Type: SEQUENCE; Schema: public; Owner: antoine
--

CREATE SEQUENCE product_package_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.product_package_seq OWNER TO antoine;

--
-- Name: product_package_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antoine
--

ALTER SEQUENCE product_package_seq OWNED BY product.package;


--
-- Name: product_product_category_seq; Type: SEQUENCE; Schema: public; Owner: antoine
--

CREATE SEQUENCE product_product_category_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.product_product_category_seq OWNER TO antoine;

--
-- Name: product_product_category_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antoine
--

ALTER SEQUENCE product_product_category_seq OWNED BY product.product_category;


--
-- Name: user; Type: TABLE; Schema: public; Owner: antoine; Tablespace: 
--

CREATE TABLE "user" (
    id integer NOT NULL,
    login character varying(32) NOT NULL,
    password character varying(40) NOT NULL
);


ALTER TABLE public."user" OWNER TO antoine;

--
-- Name: user_id_seq; Type: SEQUENCE; Schema: public; Owner: antoine
--

CREATE SEQUENCE user_id_seq
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.user_id_seq OWNER TO antoine;

--
-- Name: user_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: antoine
--

ALTER SEQUENCE user_id_seq OWNED BY "user".id;


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY fridge ALTER COLUMN id SET DEFAULT nextval('fridge_id_seq'::regclass);


--
-- Name: user_id; Type: DEFAULT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY fridge ALTER COLUMN user_id SET DEFAULT nextval('fridge_user_id_seq'::regclass);


--
-- Name: product_id; Type: DEFAULT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY fridge ALTER COLUMN product_id SET DEFAULT nextval('fridge_product_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY package ALTER COLUMN id SET DEFAULT nextval('package_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY product ALTER COLUMN id SET DEFAULT nextval('product_id_seq'::regclass);


--
-- Name: package; Type: DEFAULT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY product ALTER COLUMN package SET DEFAULT nextval('product_package_seq'::regclass);


--
-- Name: product_category; Type: DEFAULT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY product ALTER COLUMN product_category SET DEFAULT nextval('product_product_category_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY product_category ALTER COLUMN id SET DEFAULT nextval('product_category_id_seq'::regclass);


--
-- Name: id; Type: DEFAULT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY "user" ALTER COLUMN id SET DEFAULT nextval('user_id_seq'::regclass);


--
-- Data for Name: fridge; Type: TABLE DATA; Schema: public; Owner: antoine
--

COPY fridge (id, user_id, product_id) FROM stdin;
\.


--
-- Name: fridge_id_seq; Type: SEQUENCE SET; Schema: public; Owner: antoine
--

SELECT pg_catalog.setval('fridge_id_seq', 1, false);


--
-- Name: fridge_product_id_seq; Type: SEQUENCE SET; Schema: public; Owner: antoine
--

SELECT pg_catalog.setval('fridge_product_id_seq', 1, false);


--
-- Name: fridge_user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: antoine
--

SELECT pg_catalog.setval('fridge_user_id_seq', 1, false);


--
-- Data for Name: package; Type: TABLE DATA; Schema: public; Owner: antoine
--

COPY package (id) FROM stdin;
\.


--
-- Name: package_id_seq; Type: SEQUENCE SET; Schema: public; Owner: antoine
--

SELECT pg_catalog.setval('package_id_seq', 1, false);


--
-- Data for Name: product; Type: TABLE DATA; Schema: public; Owner: antoine
--

COPY product (id, rotten, total_quantity, quantity_unit, package, product_category) FROM stdin;
\.


--
-- Data for Name: product_category; Type: TABLE DATA; Schema: public; Owner: antoine
--

COPY product_category (id, category) FROM stdin;
\.


--
-- Name: product_category_id_seq; Type: SEQUENCE SET; Schema: public; Owner: antoine
--

SELECT pg_catalog.setval('product_category_id_seq', 1, false);


--
-- Name: product_id_seq; Type: SEQUENCE SET; Schema: public; Owner: antoine
--

SELECT pg_catalog.setval('product_id_seq', 1, false);


--
-- Name: product_package_seq; Type: SEQUENCE SET; Schema: public; Owner: antoine
--

SELECT pg_catalog.setval('product_package_seq', 1, false);


--
-- Name: product_product_category_seq; Type: SEQUENCE SET; Schema: public; Owner: antoine
--

SELECT pg_catalog.setval('product_product_category_seq', 1, false);


--
-- Data for Name: user; Type: TABLE DATA; Schema: public; Owner: antoine
--

COPY "user" (id, login, password) FROM stdin;
1	toto	f7e79ca8eb0b31ee4d5d6c181416667ffee528ed
13	antoine.brand@sfr.fr	a9993e364706816aba3e25717850c26c9cd0d89d
14	pablo@toto.fr	7e240de74fb1ed08fa08d38063f6a6a91462a815
\.


--
-- Name: user_id_seq; Type: SEQUENCE SET; Schema: public; Owner: antoine
--

SELECT pg_catalog.setval('user_id_seq', 14, true);


--
-- Name: fridge_pkey; Type: CONSTRAINT; Schema: public; Owner: antoine; Tablespace: 
--

ALTER TABLE ONLY fridge
    ADD CONSTRAINT fridge_pkey PRIMARY KEY (id);


--
-- Name: fridge_user_id_key; Type: CONSTRAINT; Schema: public; Owner: antoine; Tablespace: 
--

ALTER TABLE ONLY fridge
    ADD CONSTRAINT fridge_user_id_key UNIQUE (user_id);


--
-- Name: package_pkey; Type: CONSTRAINT; Schema: public; Owner: antoine; Tablespace: 
--

ALTER TABLE ONLY package
    ADD CONSTRAINT package_pkey PRIMARY KEY (id);


--
-- Name: product_category_category_key; Type: CONSTRAINT; Schema: public; Owner: antoine; Tablespace: 
--

ALTER TABLE ONLY product_category
    ADD CONSTRAINT product_category_category_key UNIQUE (category);


--
-- Name: product_category_pkey; Type: CONSTRAINT; Schema: public; Owner: antoine; Tablespace: 
--

ALTER TABLE ONLY product_category
    ADD CONSTRAINT product_category_pkey PRIMARY KEY (id);


--
-- Name: product_pkey; Type: CONSTRAINT; Schema: public; Owner: antoine; Tablespace: 
--

ALTER TABLE ONLY product
    ADD CONSTRAINT product_pkey PRIMARY KEY (id);


--
-- Name: user_login_key; Type: CONSTRAINT; Schema: public; Owner: antoine; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_login_key UNIQUE (login);


--
-- Name: user_pkey; Type: CONSTRAINT; Schema: public; Owner: antoine; Tablespace: 
--

ALTER TABLE ONLY "user"
    ADD CONSTRAINT user_pkey PRIMARY KEY (id);


--
-- Name: fridge_product_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY fridge
    ADD CONSTRAINT fridge_product_id_fkey FOREIGN KEY (product_id) REFERENCES product(id);


--
-- Name: fridge_user_id_fkey; Type: FK CONSTRAINT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY fridge
    ADD CONSTRAINT fridge_user_id_fkey FOREIGN KEY (user_id) REFERENCES "user"(id);


--
-- Name: product_package_fkey; Type: FK CONSTRAINT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY product
    ADD CONSTRAINT product_package_fkey FOREIGN KEY (package) REFERENCES package(id);


--
-- Name: product_product_category_fkey; Type: FK CONSTRAINT; Schema: public; Owner: antoine
--

ALTER TABLE ONLY product
    ADD CONSTRAINT product_product_category_fkey FOREIGN KEY (product_category) REFERENCES product_category(id);


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- PostgreSQL database dump complete
--

