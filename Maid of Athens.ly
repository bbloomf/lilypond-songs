\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Maid of Athens"}}
  composer = \markup\oldStyleNum"Lord Byron (1788–1824)"
  composer = \markup\oldStyleNum"Henry Robinson Allen (1809–1876)"
  tagline = ""
}
\paper {
  %print-all-headers = ##t
  paper-height = 11\in
  paper-width = 8.5\in
  indent = 0\in
  %system-system-spacing = #'((basic-distance . 10) (padding . 0))
  system-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . 2)
       (stretchability . 100))
  score-markup-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -14)
       (stretchability . 60))
  top-system-spacing =
    #'((basic-distance . 0)
       (minimum-distance . 0)
       (padding . -1.5)
       (stretchability . 0))
  ragged-last-bottom = ##t
  ragged-bottom = ##f
  two-sided = ##t
  inner-margin = 1\in
  outer-margin = 0.75\in
  top-margin = 0.26\in
  bottom-margin = 0.25\in
  first-page-number = #196
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
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	d'4. cis8 e d a b |
  c2. b4\rest |
  b4. a8 c a e fis |
  g2. b4\rest |
  d4. cis8 e d a b |
  
  c2. b4\rest |
  b4. a8 c a e fis |
  g2. b4\rest |
  g4. a8 b4. g8 |
  g4. a8 b2 |
  
  b4. a8 a4 b8\rest d |
  cis4( \grace {d16[ cis]} b8) cis d4. d8 |
  \tieSolid d4.( cis8[ e d]) a[ b] |
  d4 c b\rest b8\rest c |
  c4. b8 d[ c] gis[ a] |
  
  c4 b b2\rest |
  d4. f8 e b e d |
  c([ e] a4. g8) fis8.[ e16] |
  d2~ d8 d e8.\fermata d16 |
  g,2 b\rest \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Maid of Ath -- ens, ere we part,
  Give, oh, give me back my heart!
  Or, since that has left my breast,
  Keep it now, and take the rest!
  
  Hear my vow be -- fore I go,
  Hear my vow be -- fore I go,
  
  
  My life, I love thee,
  My dear -- est life, I love thee.
  Hear my vow, be -- fore I go.
  My life, I love but thee.
}

sopWordsII = \lyricmode {
  \set stanza = #"2. "
  By those tress -- es un -- con -- fined,
  Wooed by each Æ -- ge -- an wind,
  By those lids whose jet -- ty fringe,
  Kiss thy soft cheeks’ bloom -- ing tinge,
  
  By those wild eyes like the roe,
  By those wild eyes like the roe,
  
  My life, I love thee,
  My dear -- est life, I love thee.
  Hear my vow, be -- fore I go.
  My life, I love but thee.
}

sopWordsIII = \lyricmode {
  \set stanza = #"3. "
  Maid of Ath -- ens, I am gone,
  Think of me, sweet, when a -- lone,
  Though I fly to Is -- tam -- bol,
  Ath -- ens holds my heart and soul.
  
  Can I cease to love thee? No!
  Can I cease to love thee? No!
  
  
  My life, I love thee,
  My dear -- est life, I love thee!
  Can I cease to love thee? No!
  My life, I love but thee.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  g'4. g8 g g d d |
  e2. s4 |
  fis4. fis8 fis fis c c |
  b2. s4 |
  g'4. g8 g g g g |
  
  e2. s4 |
  fis4. fis8 fis fis c d |
  d2. s4 |
  e4. e8 d4. d8 |
  ees4. ees8 d2 |
  
  g4. g8 fis4 s8 fis |
  g4 g8 g fis4. fis8 |
  g2. g4 |
  e4 e s4. e8 |
  d4. d8 fis4 fis |
  
  g4 g s2 |
  e4. e8 g g g e |
  e4( c'4.)~ c8 e,4 |
  g2~ g8 g fis8. fis16 |
  g2 s \bar"|."
}
altoWords = \lyricmode {
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
  b4. ais8 c b c b |
  a2. s4 |
  d4. c8 a c fis, a |
  g2. s4 |
  b4. ais8 c b c d |
  
  a2. s4 |
  d4. d8 a c fis, c' |
  b2. s4 |
  c4. c8 g4. b8 |
  c4. c8 g2 |
  
  a4. a8 a4 s8 a |
  a4. a8 a4. c8 |
  b4.( ais8[ c b]) c[ d] |
  a4 a s4. g8 |
  fis4. fis8 a4 d |
  
  d d s2 |
  b4. b8 b b b b |
  a([ c] e4.)~ e8 c4 |
  b2~ b8 b c8. c16 |
  b2 s \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  g4. g8 g g g g |
  a,2. d4\rest |
  d4. d8 d d d d |
  g,2. d'4\rest |
  g4. g8 g g g g |
  
  a,2. d4\rest |
  d4. d8 d d d d |
  g2. d4\rest |
  c4. c8 g'4. g8 |
  c,4. c8 g'2 |
  
  cis,4. cis8 d4 d8\rest d |
  e4. e8 d4. d8 |
  g2. g4 |
  a, a d4\rest d8\rest a |
  d4. d8 d4 d |
  
  g g d2\rest |
  gis4. gis8 e e e gis |
  a4~ a4.~ a8 a4 |
  d,2~ d8 d d8.\fermata d16 |
  g2 d\rest \bar"|."
}
bassWords = \lyricmode {
}

pianoRH = \relative c' {
}
pianoLH = \relative c' {
}

\score {
<<
   \new ChoirStaff <<
    \new Staff = women <<
      \new Voice = "sopranos" { \voiceOne << \global \sopMusic >> }
      \new Voice = "altos" { \voiceTwo << \global \altoMusic >> }
    >>
    \new Lyrics = "altos"  \lyricsto "sopranos" \sopWords
    \new Lyrics = "altosII"  \lyricsto "sopranos" \sopWordsII
    \new Lyrics = "altosIII"  \lyricsto "sopranos" \sopWordsIII
    \new Lyrics = "altosIV"  \lyricsto "sopranos" \sopWordsIV
    \new Lyrics = "altosV"  \lyricsto "sopranos" \sopWordsV
   \new Staff = men <<
      \clef bass
      \new Voice = "tenors" { \voiceOne << \global \tenorMusic >> }
      \new Voice = "basses" { \voiceTwo << \global \bassMusic >> }
    >>
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsIII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWordsII
    \new Lyrics \with { alignAboveContext = #"tenors" } \lyricsto "tenors" \tenorWords
  >>
%    \new PianoStaff << \new Staff { \new Voice { \pianoRH } } \new Staff { \clef "bass" \pianoLH } >>
  >>
  \midi {
    \tempo 4 = 80
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
      \override VerticalAxisGroup #'staff-affinity = #0
      \override LyricText #'X-offset = #center-on-word
    }
    \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 8)
      \override SpacingSpanner #'common-shortest-duration = #(ly:make-moment 1 4)
    }
    \context {
      % Remove all empty staves
      \Staff \RemoveEmptyStaves \override VerticalAxisGroup #'remove-first = ##t
      
      \override VerticalAxisGroup #'staff-staff-spacing =
      #'((basic-distance . 0)
         (minimum-distance . 0)
         (padding . -1)
         (stretchability . 2))
    }
  }
}


