#!/usr/bin/env python3

import re
import sys
import os
import sqlite3
import urllib.parse
import requests
import shutil
from shutil import copyfile
from html import unescape
from bs4 import BeautifulSoup

def fatal(msg):
	print(msg, file=sys.stderr)
	sys.exit(1)

class URLUtilities:
	HTML = '../generated-docs/'
	INDEX =  HTML + 'index.html'

class HTMLUtilities:
	@staticmethod
	def find_modules(html):
		return re.findall(r'<li><a href="[^"]+">([^<]+)</a>', html)

class Generator:
	OUTPUT = 'purescript-local.docset'

	def __init__(self):
		self.version = None

	def generate(self):
		self.create_docset()
		self.create_index()
		self.save_assets()
		modules = self.fetch_index()
		print('Processing {} modules'.format(len(modules)))
		for module in modules:
			self.fetch_module(module)
		self.db.close()
		self.create_plist()
		print('Done')

	def fetch_index(self):
		self.version = None
		print('Processing module list')
		with open(URLUtilities.INDEX, 'r') as f:
			r = f.read()
		html = re.sub(r'(</a></li>)</li>', r'\1', r) # fix html error
		self.save_html(html, self.documents_path('index.html'))
		modules = HTMLUtilities.find_modules(html)
		return modules

	@staticmethod
	def create_docset():
		path = Generator.OUTPUT
		if os.path.exists(path):
			shutil.rmtree(path)
		os.makedirs(Generator.documents_path())

	def fetch_module(self, module):
		print('Processing module {}'.format(module))
		moduleFile = urllib.parse.quote(module, '') + '.html'
		with open('{}/{}'.format(URLUtilities.HTML, moduleFile), 'r') as f:
			r = f.read()
		html = self.save_html(r, self.documents_path(moduleFile))
		self.cursor.execute(
			'INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (?,?,?);',
			[module, 'Module', moduleFile])
		self.db.commit()
		return r

	@staticmethod
	def documents_path(*paths):
		return os.path.join(Generator.OUTPUT, 'Contents/Resources/Documents', *paths)

	def save_html(self, html, path):
		prefix = r''
		soup = BeautifulSoup(html, 'html.parser')
		# remove google font
		soup.find('link', href=re.compile(r'^https://fonts\.googleapis\.com/.*')).decompose()
		# remove top banner
		soup.find('div', class_='top-banner').decompose()
		aside = soup.find('div', class_='col--aside')
		if(aside):
			aside.decompose()
		# find anchors
		tlds = soup.find_all('div', class_='decl')
		for tld in tlds:
			self.process_decl(path, tld, soup)
		with open(path, 'w') as f:
			f.write(str(soup))

	def process_decl(self, path, decl, soup, type_hint = None):
		type_, name = decl.get('id').split(':', 1)
		name = unescape(name)
		if type_hint:
			type_ = type_hint
		else:
			type_ = self.convert_type(type_)
			signature = decl.find('pre', class_='decl__signature')
			if signature:
				if signature.code.find() == signature.code.find('span', class_='keyword', text='class'):
					type_ = 'Class'
		anchor_toc = '//apple_ref/cpp/{}/{}'.format(urllib.parse.quote(type_, ''), urllib.parse.quote(name, ''))
		self.cursor.execute(
			'INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (?,?,?);',
			[name, type_, '{}#{}'.format(os.path.relpath(path, self.documents_path()), anchor_toc)])
		a = soup.new_tag('a', attrs={ 'name': anchor_toc, 'class': 'dashAnchor' })
		decl.insert(0, a)
		if type_ == 'Class':
			members_lbl = decl.find('h4', text='Members')
			if members_lbl:
				for member in members_lbl.find_next_sibling().find_all('li', recursive=False):
					self.process_decl(path, member, soup, 'Function')
		elif type_ == 'Type':
			ctors_lbl = decl.find('h4', text='Constructors')
			if ctors_lbl:
				for ctor in ctors_lbl.find_next_sibling().find_all('li', recursive=False):
					self.process_decl(path, ctor, soup, 'Constructor')

	def save_assets(self):
		copyfile('favicon-16x16.png', self.documents_path('../../../icon.png'))
		copyfile('favicon-32x32.png', self.documents_path('../../../icon@2x.png'))

	@staticmethod
	def create_plist():
		with open('Info.plist.in', 'r') as f:
			plist = f.read()
		with open(Generator.documents_path('../../Info.plist'), 'w') as f:
			f.write(plist)

	@staticmethod
	def convert_type(t):
		TABLE = {
			't': 'Type',
			'v': 'Function',
			'k': 'Kind',
		}
		return TABLE[t] if t in TABLE else t

	def create_index(self):
		self.db = sqlite3.connect(self.documents_path('../docSet.dsidx'))
		self.cursor = self.db.cursor()
		self.cursor.execute('CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);')
		self.cursor.execute('CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);')

if __name__ == '__main__':
	gen = Generator()
	gen.generate()
