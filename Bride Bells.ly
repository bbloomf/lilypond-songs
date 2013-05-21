\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Bride Bells"}}
  poet = \markup\oldStyleNum"Frederic Weatherly (1848–1929)"
  composer = \markup\oldStyleNum"Joseph Leopold Röckel (1838–1923)"
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
       (padding . 1)
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
  first-page-number = #82
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
  \key aes \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  %\set midiInstrument = "recorder"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
	b'2\rest b4\rest ees, |
  c' aes g f |
  aes4. ees8 ees4 aes |
  bes bes c des |
  c2. c4 |
  c bes aes c |
  
  \tieDashed
  bes4. aes8 g4~ g8 g |
  \tieSolid
  f4 g aes bes |
  \slurDashed
  c2 d4( ees) |
  \slurSolid
  bes8 g4. \acciaccatura g8 f4. ees8 |
  ees2. ees4 \bar"||"
  
  aes2-- aes-- |
  aes2.-- ees4 |
  aes bes c f |
  ees des bes2 |
  aes-- aes-- |
  aes1 |
  
  aes2\pp aes |
  aes1~ |
  aes2.\fermata ees4 |
  f\< g aes bes |
  c\! des ees aes,\f |
  c2^\markup\italic"rit." \acciaccatura {aes16[ c]} bes4.( aes8) |
  aes2.\fermata \bar"||" 
  \once \override Score.RehearsalMark #'break-visibility = #end-of-line-visible
  \once \override Score.RehearsalMark #'self-alignment-X = #RIGHT
  \mark \markup\smallCapsOldStyle"Fine."
  
  \break
  aes4 |
  aes^\markup\italic"piu lento" des c bes |
  aes f bes4. f8 |
  aes4 ges f ees |
  
  f2. f4 |
  f\< des'\! des\> c\! |
  f,\< des' des c\! |
  
  %page2/453
  f des c bes |
  aes2. aes4 |
  g^\markup\italic"molto rit." g \acciaccatura bes8 aes4. g8 |
  
  g2.\fermata\> ees4 |
  aes2--\p aes-- |
  aes2.-- ees4 |
  aes bes\< c f\! |
  ees\> des bes2\! |
  aes2\mf aes |
  
  aes1 |
  aes2\pp aes |
  aes1~ |
  aes2. ees4 |
  f\< g aes bes |
  c des\! ees\fermata f\f |
  c2 bes4.( aes8) |
  aes2. \bar"|."
}
sopWords = \lyricmode {
  \set stanza = #"1. "
	Maid El -- sie roams by lane and lea,
  Her heart beats low and sad,
  \set ignoreMelismata = ##t
  Her thoughts are far a -- way at sea,
  With her bon -- nie sai -- lor lad,
  With her bon -- nie sai -- lor lad.
  \unset ignoreMelismata
  But Kling, lang, ling,
  She seems to hear her bride bells ring,
  Kling, lang, ling,
  Kling, lang, ling, __
  She seems to hear her bride bells ring, her bride bells ring!
  
  
  \set stanza = #"2. "
  That night her lov -- er’s good ship rode
  The fu -- rious Bis -- cay foam,
  And as the stream -- ing deck he trod,
  He thought of her at home,
  He thought of her at home;
  While Kling, lang, ling,
  He seem’d to hear his home bells ring!
  Kling, lang, ling,
  Kling, lang, ling, __
  He seem’d to hear his home bells ring, his home bells ring!
}

sopWordsII = \lyricmode {
  \set stanza = #"3. "
  A year by seas, a year by lands,
  A year since then has died.
  And El -- sie at the al -- tar stands,
  Her sai -- lor at her side,
  Her __ sai -- lor at her side,
  While Kling, lang, ling,
  Their bon -- nie bride bells gai -- ly ring,
  Kling, lang, ling,
  Kling, lang, ling, __
  Their bon -- nie bride bells gai -- ly ring, their bride bells ring!
}

sopWordsIII = \lyricmode {
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  s2. ees4 |
  ees c b b |
  c4. c8 c4 c |
  ees ees ees ees |
  ees2. ees4 |
  e g f f |
  
  \tieDashed
  f4. f8 ees4~ ees8 ees |
  \tieSolid
  c4 e f ees |
  \slurDashed
  ees2 ees4( ees) |
  \slurSolid
  g8 ees4. d ees8 |
  ees2. ees4 \bar"||"
  
  c2 des |
  c2. c4 |
  c ees ees aes |
  g g g2 |
  ees f |
  ees1 |
  
  ees2 f |
  ees1~ |
  ees2. ees4 |
  des des c ees |
  ees ees ees f |
  ees2 des |
  c2. \bar"||"
  
  f4 |
  f f ges ges |
  f des f4. d8 |
  ees4 ees c c |
  
  des2. c4 |
  des f ges ges |
  des f ges ges |
  
  f f f f |
  f2. f4 |
  des des d4. d8 |
  
  c2. des4 |
  c2 des |
  c2. ees4 |
  c ees ees aes |
  g g g2 |
  ees f |
  
  ees1 |
  ees2 f |
  ees1~ |
  ees2. c4 |
  des des c ees |
  ees ees ees aes |
  aes2 g |
  aes2. \bar"|."
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
  s2. ees,4 |
  aes aes des, des |
  ees4. aes8 aes4 aes |
  g g g g |
  aes2. aes4 |
  g c c aes |
  
  \tieDashed
  bes4. bes8 bes4~ bes8 bes |
  \tieSolid
  aes4 c c g |
  \slurDashed
  aes2 fis4( fis) |
  \slurSolid
  g8 bes4. aes g8 |
  g2. g4 \bar"||"
  
  aes2 f |
  aes2. aes4 |
  aes g aes c |
  bes bes ees2 |
  c des |
  c1 |
  
  c2 des |
  c1~ |
  c2. aes4 |
  aes ees ees g |
  aes bes c bes |
  aes2 g |
  aes2. \bar"||"
  
  aes4 |
  aes aes bes c |
  des aes bes4. bes8 |
  ges4 bes aes aes |
  
  aes2. a4 |
  bes bes a a |
  bes bes a a |
  
  bes bes des des |
  c2. f,4 |
  f f f4. f8 |
  
  e2. g4 |
  aes2 f |
  aes2. g4 |
  aes g aes c |
  bes bes ees2 |
  c des |
  
  c1 |
  c2 des |
  c1~ |
  c2. aes4 |
  aes ees ees g |
  aes bes c des |
  c2 des |
  c2. \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  d,2\rest d4\rest ees |
  aes, aes aes aes |
  aes4. aes8 aes4 aes |
  ees' ees ees ees |
  aes,2. aes4 |
  c e f f |
  
  \tieDashed
  d4. d8 ees4~ ees8 ees |
  \tieSolid
  aes4 g f ees |
  \slurDashed
  aes,2 a4( a) |
  \slurSolid
  bes8 bes4. bes ees8 |
  ees2. ees4 \bar"||"
  
  aes,2 aes |
  aes2. aes4 |
  aes ees' ees ees |
  ees ees ees2 |
  aes aes |
  aes1 |
  
  aes2 aes |
  aes1~ |
  aes2. c,4 |
  des bes aes ees' |
  aes bes c d, |
  ees2 ees |
  aes,2. \bar"||"
  
  des4 |
  des des des des |
  des des d4. d8 |
  ees4 ees aes, aes |
  
  des2. f4 |
  bes, bes bes bes |
  bes bes bes bes |
  
  des4 bes bes bes |
  c2. c4 |
  bes bes b4. b8 |
  
  c2. ees4 |
  aes,2 aes |
  aes2. ees'4 |
  aes, ees' aes, aes' |
  ees ees ees2 |
  aes aes |
  
  aes1 |
  aes2 aes |
  aes1~ |
  aes2. aes,4 |
  des bes aes ees' |
  aes bes c des, |
  ees2 ees |
  aes2. \bar"|."
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
    \tempo 4 = 110
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


