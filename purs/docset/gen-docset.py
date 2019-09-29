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

class Generator:
	GENERATED_DOCS = '../generated-docs/html'
	OUTPUT = 'purescript-local.docset'

	def documents_path(self, *paths):
		return os.path.join(self.OUTPUT, 'Contents/Resources/Documents', *paths)

	def generate(self):
		self.create_docset()
		self.create_index()
		self.save_assets()
		print('Processing index')
		modules = self.process_index()
		print('Processing {} modules'.format(len(modules)))
		for module in modules:
			print('Processing module {}'.format(module))
			self.process_module(module)
		self.db.close()
		self.create_plist()
		print('Done')

	def save_assets(self):
		copyfile('favicon-16x16.png', self.documents_path('../../../icon.png'))
		copyfile('favicon-32x32.png', self.documents_path('../../../icon@2x.png'))

	def create_docset(self):
		path = self.OUTPUT
		if os.path.exists(path):
			shutil.rmtree(path)
		os.makedirs(self.documents_path())

	def create_index(self):
		self.db = sqlite3.connect(self.documents_path('../docSet.dsidx'))
		self.cursor = self.db.cursor()
		self.cursor.execute('CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT);')
		self.cursor.execute('CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path);')

	def create_plist(self):
		with open('Info.plist.in', 'r') as f:
			plist = f.read()
		with open(self.documents_path('../../Info.plist'), 'w') as f:
			f.write(plist)

	def find_modules(self, html):
		return re.findall(r'<li><a href="[^"]+">([^<]+)</a>', html)

	def process_index(self):
		with open('{}/{}'.format(self.GENERATED_DOCS, 'index.html'), 'r') as f:
			i = f.read()
		i_html = re.sub(r'(</a></li>)</li>', r'\1', i) # fix html error
		self.save_html(i_html, self.documents_path('index.html'))
		return self.find_modules(i_html)

	def process_module(self, module):
		moduleFile = urllib.parse.quote(module, '') + '.html'
		with open('{}/{}'.format(self.GENERATED_DOCS, moduleFile), 'r') as f:
			m_html = f.read()
		self.save_html(m_html, self.documents_path(moduleFile))
		self.insert_search_index(module, 'Module', moduleFile)
		self.db.commit()

	def save_html(self, html, path):
		soup = BeautifulSoup(html, 'html.parser')
		# remove google font
		soup.find('link', href=re.compile(r'^https://fonts\.googleapis\.com/.*')).decompose()
		# remove top banner
		soup.find('div', class_='top-banner').decompose()
		# remove sidebar
		aside = soup.find('div', class_='col--aside')
		if aside: aside.decompose()
		# find anchors
		top_decls = soup.find_all('div', class_='decl')
		for top_decl in top_decls:
			self.process_decl(path, top_decl, soup)
		with open(path, 'w') as f:
			f.write(str(soup))

	def process_decl(self, path, decl, soup, type_hint = None):
		type_, name = self.get_decl_type(decl, type_hint)

		anchor_toc = self.to_anchor_toc(type_, name)
		decl.insert(0, soup.new_tag('a', attrs={ 'name': anchor_toc, 'class': 'dashAnchor' }))
		
		self.insert_search_index(name, type_,
			'{}#{}'.format(os.path.relpath(path, self.documents_path()), anchor_toc))

		if type_ == 'Class':
			members_lbl = decl.find('h4', text='Members')
			if members_lbl:
				for member_decl in members_lbl.find_next_sibling().find_all('li', recursive=False):
					self.process_decl(path, member_decl, soup, 'Function')
		elif type_ == 'Type':
			ctors_lbl = decl.find('h4', text='Constructors')
			if ctors_lbl:
				for ctor_decl in ctors_lbl.find_next_sibling().find_all('li', recursive=False):
					self.process_decl(path, ctor_decl, soup, 'Constructor')

	def get_decl_type(self, decl, type_hint = None):
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
		return type_, name

	def to_anchor_toc(self, type_, name):
		return '//apple_ref/cpp/{}/{}'.format(urllib.parse.quote(type_, ''), urllib.parse.quote(name, ''))

	def insert_search_index(self, name, type_, path):
		self.cursor.execute('INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (?,?,?);',
			[name, type_, path])
                
	def convert_type(self, t):
		TABLE = {
			't': 'Type',
			'v': 'Function',
			'k': 'Kind',
		}
		return TABLE[t] if t in TABLE else t

if __name__ == '__main__':
	gen = Generator()
	gen.generate()
