create table "objects"("hash" text not null, "version" bigint not null, "path" hstore not null, "value" hstore not null);

create unique index "objects_hash" on "objects" ("hash");
create index "objects_path" on "objects" using gist ("path");
create index "objects_value" on "objects" using gist ("value");
