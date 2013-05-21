\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Hail! Smiling Morn"}}
  composer = \markup\oldStyleNum"Reginald Spofforth (1769–1827)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . -1)
       (minimum-distance . -1)
       (padding . -3)
       (stretchability . 100))
  top-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 0))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 50))
  last-bottom-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 0)
       (stretchability . 50))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 70))
  ragged-last-bottom = ##f
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #1
  print-first-page-number = ##t
  headerLine = ""
  oddHeaderMarkup = \markup\fill-line{
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine 
        \fill-line{"" \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        }
        \fill-line{\headerLine}
  }
  evenHeaderMarkup = \markup {
     \override #'(font-name . "Garamond Premier Pro")\abs-fontsize #12.5
     \combine
        \on-the-fly #print-page-number-check-first
        \oldStylePageNum""
        \fill-line{\headerLine}
  }
}
#(set-global-staff-size 18) \paper{ #(define fonts (make-pango-font-tree "Garamond Premier Pro" "Garamond Premier Pro" "Garamond Premier Pro" (/ 18 20))) }
global = {
  \key g \major
  \time 6/8
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c'' {
	\repeat volta 2 {
    g4.~\f g8[ b g] d4. d8[ e] fis g4. fis8[ g] a g4.~g4
    d8 e4 g8 g[ fis] e d4.~d4 b8\p c4 e8 e[ d] c b4.~b4 a8\f
    b4. cis4. d4 fis8 e4 g8 fis4. e4. 
    fis8[ e fis] a[ gis a] e4.~e8[ fis g] fis[ e fis] a[ gis a] e4.~e8[ fis g] fis4.
    g4 b8 a4 g8 fis4 e8 fis4. b4. a2. a
  } \pageBreak
  \repeat volta 2 {
    g4\f g8 g4. g~g4 b8 b4 g8 fis[ g] a g2.~g~g~g~g4.~g4 g8
    g4. a fis8 g4 r4. a4\p a8 g4 fis8 g4. b4 g8 d2.~d4. b'4\pp g8 d2.~d4.
    d\cresc~d4\! d8 c4 d8 e4. e~e4 d8 fis4 g8 g4.( fis4) fis8 
    g4. a b8 g4 r4. e4.\fz a\fz r2. d,4.\p~d4 b8 g'4.~g fis4 g8 a4 fis8 g4. b4\cresc g8\!
    fis8[ g fis] a[ g fis] g[ a g] b[ a g] fis8[ g fis] a[ g fis] g4. g\f a fis g g a fis g2.  
  }
}
sopWords = \lyricmode {
	Hail, __ _ smil -- ing morn, smil -- ing morn
	that tips the hills with gold,
	that tips the hills with gold,
	whose ro -- sy fin -- gers ope the gates of day, __ _ _ _ _ _ _
	ope the gates, the gates of day, Hail Hail Hail!
	
  Who the gay face of na -- ture doth un -- fold, __
	at whose bright pres -- ence,
	dark -- ness flies a -- way, flies a -- way, __ flies a -- way, __
	dark -- ness flies a -- way, dark -- ness flies a -- way,
	at whose bright pres -- ence,
	dark -- ness flies __ _ _ _ _ _ a -- way, flies a -- way, __ _ _ _ _ _ _ 
	Hail Hail Hail Hail Hail Hail Hail Hail!
}

altoMusic = \relative c' {
\repeat volta 2 {
	d4.\f r4. b4. a4 d8 d4. d4 d8 d4.~d4
	b8 c4 e8 e[ d] c b4.~b4 d8\p c4 c8 a4 a8 g4.~g4 r8
	r4\f g'8 g4 g8 fis4 d8 e4 e8 d4. cis4. 
	d8[ e d]~d4. cis4.~cis8[ d cis] d[ e d]~d4. cis4.~cis8[ d cis] d4.
	d4 d8 d4 e8 d4 cis8 d4. g4. fis4. e4. fis2.
	}
\repeat volta 2 {
	d4\f d8 d4. d~d4 g8 g4 d8 d4 d8 d2. e4 e8 e4. e~e4 e8 e4 e8 d[ e] f e4.~e4 e8
	e4. e fis?8 e4 r4. e4\p e8 d4 d8 d4. r r c8[ b] a b4. r r\pp c8[ b] a b4. 
	b~b4 b8 c4 b8 c4. c~c4 d8 d4 d8 d4.~d4 d8
	d4. d d8 d4 r4. c\fz e\fz r2. b2.~b4 g8 d'4.~d~d4 d8 d4.
	r d4 d8 d4 d8 d4. r d4 d8 d4 d8 d4. e\f~e d d e~e d d2.  
	}
}
altoWords = \lyricmode {
	Hail, Hail, smil -- ing morn, smil -- ing morn
	that tips the hills with gold,
	that tips the hills with gold,
	whose ro -- sy fin -- gers ope the gates of day, __ _ _ _ _
	ope the gates, the gates of day, Hail Hail Hail Hail!
	
  Who the gay face of na -- ture doth un -- fold,
	Who the gay face of na -- ture doth un -- fold,
	at whose bright pres -- ence,
	dark -- ness flies a -- way, flies a -- way, flies a -- way,
	dark -- ness flies a -- way, dark -- ness flies a -- way,   
	at whose bright pres -- ence,
	dark -- ness flies __ _ _ a -- way, 
	dark -- ness flies a -- way, dark -- ness flies a -- way, 
	Hail Hail Hail Hail Hail Hail!
}
altoWordsII = \lyricmode {
%\markup\italic
  \set stanza = #"2. "
}
altoWordsIII = \lyricmode {
  \set stanza = #"3. "
}
altoWordsIV = \lyricmode {
  \set stanza = #"4. "
}
altoWordsV = \lyricmode {
  \set stanza = #"5. "
  \set ignoreMelismata = ##t
}
altoWordsVI = \lyricmode {
  \set stanza = #"6. "
  \set ignoreMelismata = ##t
}
tenorMusic = \relative c' {
\repeat volta 2 {
	b4.\f r4. g4. fis8[ g] a g4. a8[ b] c8 b4.~b4
	g8 g4 g8 g4 g8 g4.~g4 g8\p g4 g8 a4 a8 b4.~b4 r8
	r4\f g8 g4 g8 a4 a8 b4 b8 a4. a a2.~a a4.~a4 a8 a4.~a4 a8 a4.
	b4 g8 fis4 g8 a4 a8 d4. d d cis d2. 
	}
\repeat volta 2 {
	b4\f b8 b4. b~b4 d8 d4 b8 a[ b] c b2. c4 c8 c4. c~c4 c8 c4 c8 b[ c] d c4.~c4 c8
	c4. c c8 b4 r4. c4\p c8 b4 a8 b4. r r a8[ g] fis g4. r r\pp a8[ g] fis g4.
	g~g4 g8 g4 g8 g4. c~c4 b8 a4 b8 b4.( a4) a8
	g4. fis g8 g4 r4. g\fz c\fz r2. g2. b a4 b8 c4 a8 b4. 
	d4\cresc b8\! a[ b a] c[ b a] b[ c b] d[ c b] a[ b a] c[ b a] b4. b\f c a b b c a b2.
	}
}
tenorWords = \lyricmode {
	Hail, Hail, smil -- ing morn, smil -- ing morn,
	that tips the hills with gold,
	that tips the hills with gold,
	whose ro -- sy fin -- gers ope the gates of day, __ ope the gates of day,
	ope the gates, the gates of day, Hail Hail Hail Hail!
	
  Who the gay face of na -- ture doth un -- fold,
	Who the gay face of na -- ture doth un -- fold,
	at whose bright pres -- ence,
	dark -- ness flies a -- way, flies a -- way, flies a -- way,
	dark -- ness flies a -- way, dark -- ness flies a -- way,   
	at whose bright pres -- ence, 
	dark -- ness flies __ _ _ _ _ a -- way, flies a -- way, __ _ _ _ _ _ _
	Hail Hail Hail Hail Hail Hail Hail Hail!

}

bassMusic = \relative c' {
\repeat volta 2 {
	g4.\f r4. g4. d4 d8 b4. d4 d8 g2.~g~g4.~g4
	g8\p e4 e8 fis4 fis8 g4.~g4 fis8\f
	g4. e fis4 fis8 g4 g8 a4. a,
	d8[ cis d] fis[ e fis] a[ b a] g[ fis e] d[ cis d] fis[ e fis] a[ b a] g[ fis e] d4.
	b4 g'8 fis4 g8 a4 a8 d4. g, a a, d2.
	}
\repeat volta 2 {
	g4\f g8 g4. g~g4 g,8 g4 b8 d4 d8 g2. c4 c8 c4. c~c4 c,8 e4 e8 g4 g8 c4.~c4 c8
	c4. c, d8 e4 r4. c4\p c8 d4 d8 g4. r r d4 d8 g4. r r\pp d4 d8 g4.
	g~g4 f8 e4 d8 c4. c'~c4 b8 a4 g8 d4.~d4 c8
	b4. a g8 b4 r4. c\fz a\fz r2. d2.~d~d4.~d4 d8 g4. r
	d4 d8 d4 d8 g4. r d4 d8 d4 d8 g4. e\f c d g e c d g,2.  
	}
}
bassWords = \lyricmode {
	Hail, Hail, smil -- ing morn, smil -- ing morn, __
	that tips the hills with gold,
	whose ro -- sy fin -- gers ope the gates of day, __ _ _ _ _ _ _ _ _
	ope the gates, the gates of day, Hail Hail Hail Hail!
  
	Who the gay face of na -- ture doth un -- fold,
	Who the gay face of na -- ture doth un -- fold,
	at whose bright pres -- ence,
	dark -- ness flies a -- way, flies a -- way, flies a -- way,
	dark -- ness flies a -- way, dark -- ness flies a -- way,   
	at whose bright pres -- ence,
	dark -- ness flies __ a -- way, 
	dark -- ness flies a -- way, dark -- ness flies a -- way, 
	Hail Hail Hail Hail Hail Hail Hail Hail!

}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
  <<
    \new Staff = "sopranos" \new Voice = "sopranos" { << \global \sopMusic >> }
    \new Staff = "altos" \new Voice = "altos" { << \clef "treble_8" \global \altoMusic >> }
    \new Lyrics \with { alignBelowContext = #"sopranos" } \lyricsto "sopranos" \sopWords
    \new Lyrics \with { alignBelowContext = #"altos" } \lyricsto "altos" \altoWords
    \new Staff = tenors { \clef "treble_8" \new Voice = "tenors" { << \global \tenorMusic >> } }
    \new Staff = basses { \clef bass \new Voice = "basses" { << \global \bassMusic >> } }
    
    \new Lyrics \with { alignBelowContext = #"tenors" } \lyricsto "tenors" \tenorWords
    \new Lyrics \with { alignBelowContext = #"basses" } \lyricsto "basses" \bassWords
  >>
    %\new PianoStaff << \new Staff { \new Voice { \global \pianoRH } } \new Staff { \clef "bass" \global \pianoLH } >>
  \midi {
    \tempo 4 = 180
    \set Staff.midiInstrument = "flute"
  
    \context {
      \Voice
      \remove "Dynamic_performer"
    }
  }
  \layout {
  \context {
    \Lyrics
    \override LyricText #'font-size = #1.3
    \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      % \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
    }
  }
}


