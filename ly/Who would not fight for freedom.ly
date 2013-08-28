\version "2.14.2"
\include "util.ly"
\header {
  title = \markup{\override #'(font-name . "Garamond Premier Pro Semibold"){ \abs-fontsize #18 \smallCapsOldStyle"Who would not fight for freedom?"}}
  composer = \markup\oldStyleNum"Old Scotch Air"
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
       (padding . -5)
       (stretchability . 100))
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
  \key ees \major
  \time 4/4
  \dynamicUp
  %\set crescendoSpanner = #'dashed-line
  \set midiInstrument = #"flute"
  \autoBeamOff
  \override DynamicTextSpanner #'style = #'none
}

sopMusic = \relative c' {
  \set midiInstrument = #"acoustic grand"
  <ees bes>4 <ees c>8.[ <f d>16] << {g8.[ f16] g16[ bes8.] } \\ {ees,4 d} >> |
  <c ees aes c>4->_\markup\italic"poco rit." <ees aes ees'>-> <g bes ees g>2->\fermata
  \set midiInstrument = #"flute"
  ees4 ees8. f16 g8. f16 g bes8. |
  
  ees,4 ees8. aes16 g8. bes16 f4 |
  ees ees8. f16 g8. f16 g bes8. |
  c8 ees bes ees g, ees' f,4 |
  
  g8 bes bes c16[ d] ees8 bes c bes |
  g bes bes c16[ d] ees8 g, f4 |
  
  g8 bes bes c16[ d] ees8 bes c bes |
  c8 ees bes8 ees g, ees' f,4 \bar"||"
  
  ees4 ees8. f16 g8. f16 g bes8. |
  ees,4 ees8. aes16 g8. bes16 f4 |
  
  ees ees8. f16 g8. f16 g bes8. |
  c8 ees bes8 g f8. g16 ees4\fermata \bar"|."
  
}
sopWords = \lyricmode {
  \repeat unfold 5 \skip1
  \set stanza = #"1. "
  Who would not fight for Free -- dom?
  Who would not draw the sword?
  Who would not up and ral -- ly
  At the great Re -- pub -- lic’s word?
  It -- a -- ly’s fair plains are rav -- aged,
  Ven -- ice threat -- en’d by the Hun,
  Quick -- ly let us cross the o -- cean
  Ere the cru -- el deed is done.
}

sopWordsII = \lyricmode {
  \set stanza = \markup\dynamic"f"
  \repeat unfold 5 \skip1
  \set stanza = #"2. "
  Who would not fight for Bel -- gium?
  Who would not fight for France?
  Who would not stand with Eng -- land
  To re -- pel the foe’s ad -- vance?
  We have heard their wo -- men call -- ing
  For our help a -- cross the sea,
  We have heard their weep -- ing chil -- dren;
  Come and fight and set them free.
  
  Who would not fight for Free -- dom?
  Who would not draw the sword?
  Who would not up and ral -- ly
  At the great Re -- pub -- lic’s word?
}

sopWordsIII = \lyricmode {
  \repeat unfold 5 \skip1
  \set stanza = #"3. "
  Who would not fight the Prus -- sian?
  What man would be a slave?
  Up, then, let ev -- ’ry free -- man
  Fight, his coun -- try’s life to save.
  Ev -- ’ry man whose heart is loy -- al,
  Ev -- ’ry man of cour -- age tried,
  Let him heed his coun -- try’s sum -- mons,
  Let him stand on Free -- dom’s side.
}

sopWordsIV = \lyricmode {
  \set stanza = #"4. "
}

sopWordsV = \lyricmode {
  \set stanza = #"5. "
}

altoMusic = \relative c' {
  s1*2 |
  bes4 c8. d16 ees8. ees16 d16 d8. |
  
  c4 bes8. d16 ees8. ees16 d4 |
  bes4 c8. d16 ees8. d16 d16 g8. |
  aes8 aes ees ees ees ees d4 |
  
  ees8 ees ees g g g ees ees |
  ees8 ees ees g g ees d4 |
  
  ees8 ees ees g g g ees ees |
  aes8 aes ees8 ees ees ees d4 |
  
  bes4 c8. d16 ees8. ees16 d16 d8. |
  c4 bes8. d16 ees8. ees16 d4 |
  
  bes4 c8. d16 ees8. d16 d16 ees8. |
  aes8 aes g8 ees c d bes4 \bar"|."
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
  s1*2 |
  g4 g8. bes16 bes8. bes16 bes bes8. |
  
  aes4 bes8. bes16 bes8. bes16 bes4 |
  g g8. bes16 bes8. bes16 bes bes8. |
  ees8 c bes bes bes bes bes4 |
  
  bes8 g g g bes bes g g |
  bes g g g bes bes bes4 |
  
  bes8 g g g bes bes g g |
  ees'8 c bes bes bes bes bes4 |
  
  g4 g8. bes16 bes8. bes16 bes bes8. |
  aes4 bes8. bes16 bes8. bes16 bes4 |
  
  g g8. bes16 bes8. bes16 bes bes8. |
  ees8 c ees bes aes g g4 \bar"|."
}

tenorWords = \lyricmode {
}

tenorWordsII = \lyricmode {
}

tenorWordsIII = \lyricmode {
}

bassMusic = \relative c' {
  \override DynamicLineSpanner #'Y-extent = #'(-6 . 0)
  \set midiInstrument = #"acoustic grand"
  <g ees>4 <g c,>8.[ <bes bes,>16] <bes ees,>4 <bes g> |
  <aes aes,>4->\< <c aes>-> <bes ees,>2->\fermata\!
  \set midiInstrument = #"flute"
  ees,4 c8. bes16 ees8. ees16 g g8. |
  
  aes4 g16 f8. ees8. ees16 bes4 |
  ees c8. bes16 ees8. ees16 g g8. |
  aes8 aes g g ees ees bes4 |
  
  ees8 ees ees ees16[ d] ees8 ees c ees 
  ees8 ees ees ees16[ d] ees8 g, bes4 |
  
  ees8 ees ees ees16[ d] ees8 ees c ees |
  aes8 aes g g ees ees bes4 \bar"||"
  
  ees4 c8. bes16 ees8. ees16 g g8. |
  aes4 g8 f ees8. ees16 bes4 |
  
  ees c8. bes16 ees8. ees16 g g8. |
  aes8 aes g g, aes bes ees4 \bar"|."
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
    \tempo 4 = 90
    \context {
      \Staff
      \remove "Staff_performer"
    }
    \context {
      \Voice
      \consists "Staff_performer"     
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
