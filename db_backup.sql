--
-- PostgreSQL database dump
--

-- Dumped from database version 14.8 (Ubuntu 14.8-0ubuntu0.22.04.1)
-- Dumped by pg_dump version 14.8 (Ubuntu 14.8-0ubuntu0.22.04.1)

SET statement_timeout = 0;
SET lock_timeout = 0;
SET idle_in_transaction_session_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SELECT pg_catalog.set_config('search_path', '', false);
SET check_function_bodies = false;
SET xmloption = content;
SET client_min_messages = warning;
SET row_security = off;

SET default_tablespace = '';

SET default_table_access_method = heap;

--
-- Name: my_table; Type: TABLE; Schema: public; Owner: ruser
--

CREATE TABLE public.my_table (
    id integer NOT NULL,
    name1 text NOT NULL,
    name2 text NOT NULL,
    dat text NOT NULL,
    proof text
);


ALTER TABLE public.my_table OWNER TO ruser;

--
-- Name: my_table_backup; Type: TABLE; Schema: public; Owner: postgres
--

CREATE TABLE public.my_table_backup (
    id integer,
    name1 text,
    name2 text,
    dat text,
    proof text
);


ALTER TABLE public.my_table_backup OWNER TO postgres;

--
-- Name: my_table_id_seq; Type: SEQUENCE; Schema: public; Owner: ruser
--

CREATE SEQUENCE public.my_table_id_seq
    AS integer
    START WITH 1
    INCREMENT BY 1
    NO MINVALUE
    NO MAXVALUE
    CACHE 1;


ALTER TABLE public.my_table_id_seq OWNER TO ruser;

--
-- Name: my_table_id_seq; Type: SEQUENCE OWNED BY; Schema: public; Owner: ruser
--

ALTER SEQUENCE public.my_table_id_seq OWNED BY public.my_table.id;


--
-- Name: my_table id; Type: DEFAULT; Schema: public; Owner: ruser
--

ALTER TABLE ONLY public.my_table ALTER COLUMN id SET DEFAULT nextval('public.my_table_id_seq'::regclass);


--
-- Data for Name: my_table; Type: TABLE DATA; Schema: public; Owner: ruser
--

COPY public.my_table (id, name1, name2, dat, proof) FROM stdin;
1	Petir	Anna	2022-12-02	Jon
2	Mike	Anna	2023-02-12	John
3	Norman	Borman	2021-10-01	Johan
4	Borman	Norman	2000-01-01	Joe
5	Xi	Petir	1989-07-26	Ju
6	Xi	Anna	2023-07-26	Go
\.


--
-- Data for Name: my_table_backup; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY public.my_table_backup (id, name1, name2, dat, proof) FROM stdin;
1	Petir	Anna	2022-12-02	Jon
2	Mike	Anna	2023-02-12	John
3	Norman	Borman	2021-10-01	Johan
4	Borman	Norman	2000-01-01	Joe
5	Xi	Petir	1989-07-26	Ju
6	Xi	Anna	2023-07-26	Go
\.


--
-- Name: my_table_id_seq; Type: SEQUENCE SET; Schema: public; Owner: ruser
--

SELECT pg_catalog.setval('public.my_table_id_seq', 6, true);


--
-- Name: my_table my_table_pkey; Type: CONSTRAINT; Schema: public; Owner: ruser
--

ALTER TABLE ONLY public.my_table
    ADD CONSTRAINT my_table_pkey PRIMARY KEY (id);


--
-- PostgreSQL database dump complete
--

